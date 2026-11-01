// sc_lsp: a language server for SuperCollider, over stdio.
//   sc_lsp [--library PATH]... [--exclude PATH]... [--interpreted-version 3.16|legacy] [--sclang-port N]
//          [--no-hooks] [--threads N]
// Without --library (or the libraryPaths initialization option) it uses the class library installed beside it and
// the default Extensions directories.

#include "sc_query/document.hpp"
#include "sc_query/hooks.hpp"
#include "sc_query/jsonrpc.hpp"
#include "sc_query/library.hpp"
#include "sc_query/queries.hpp"
#include "sc_source/normalise_source.hpp"
#include "sc_source/text_info.hpp"

#include <algorithm>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <deque>
#include <future>
#include <iostream>
#include <map>
#include <mutex>
#include <optional>
#include <set>
#include <thread>

namespace {

using namespace sc::query;
using Clock = std::chrono::steady_clock;

constexpr const char* version = "0.1.0";
constexpr auto analysis_delay = std::chrono::milliseconds { 120 };
constexpr auto rebuild_delay = std::chrono::milliseconds { 600 };
constexpr auto disk_check_interval = std::chrono::seconds { 5 };

void log(const std::string& line) { std::cerr << "[sc_lsp] " << line << std::endl; }

struct Options {
    LibraryConfig library;
    bool library_given { false };
    HookHostConfig hooks;
    bool hooks_enabled { true };
};

class Server {
public:
    Server(rpc::Transport& transport, Options options, std::filesystem::path executable):
        m_transport(transport),
        m_options(std::move(options)),
        m_executable(std::move(executable)) {}

    int run() {
        std::thread reader([this] {
            while (auto message = m_transport.read()) {
                std::lock_guard lock { m_lock };
                m_inbox.push_back(std::move(*message));
                m_wake.notify_one();
            }
            std::lock_guard lock { m_lock };
            m_input_closed = true;
            m_wake.notify_one();
        });
        reader.detach();

        while (!m_exit) {
            std::optional<Json> message;
            {
                std::unique_lock lock { m_lock };
                const auto deadline = next_deadline();
                const auto ready = [&] { return !m_inbox.empty() || m_input_closed; };
                if (deadline)
                    m_wake.wait_until(lock, *deadline, ready);
                else
                    m_wake.wait_for(lock, std::chrono::seconds { 1 }, ready);
                if (!m_inbox.empty()) {
                    message = std::move(m_inbox.front());
                    m_inbox.pop_front();
                } else if (m_input_closed) {
                    break;
                }
            }
            if (message)
                handle(*message);
            timers();
        }
        if (m_rebuild.valid())
            m_rebuild.wait();
        return m_shutdown ? 0 : 1;
    }

private:
    ////////////////////////////////////////////////////////////////////////////////
    // Messages
    ////////////////////////////////////////////////////////////////////////////////

    void handle(const Json& message) {
        const auto& method = message["method"].str();
        const auto& id = message["id"];
        const bool is_request = message.contains("id") && !method.empty();
        if (method.empty())
            return; // a response to a request of ours
        if (!m_initialized && method != "initialize" && method != "exit") {
            if (is_request)
                m_transport.respond_error(id, rpc::error_code::server_not_initialized, "Not initialized");
            return;
        }
        try {
            const auto& params = message["params"];
            if (method == "initialize")
                return m_transport.respond(id, initialize(params));
            if (method == "initialized")
                return;
            if (method == "shutdown") {
                m_shutdown = true;
                return m_transport.respond(id, Json {});
            }
            if (method == "exit") {
                m_exit = true;
                return;
            }
            if (method == "textDocument/didOpen")
                return did_open(params);
            if (method == "textDocument/didChange")
                return did_change(params);
            if (method == "textDocument/didSave")
                return did_save(params);
            if (method == "textDocument/didClose")
                return did_close(params);
            if (method == "workspace/didChangeWatchedFiles") {
                schedule_rebuild(Clock::now());
                return;
            }
            if (method == "$/cancelRequest" || method == "$/setTrace" || method == "workspace/didChangeConfiguration")
                return;
            if (!is_request)
                return;
            if (method == "textDocument/hover")
                return m_transport.respond(id, on_hover(params));
            if (method == "textDocument/definition")
                return m_transport.respond(id, on_definition(params));
            if (method == "textDocument/references")
                return m_transport.respond(id, on_references(params));
            if (method == "textDocument/completion")
                return m_transport.respond(id, on_completion(params));
            if (method == "textDocument/signatureHelp")
                return m_transport.respond(id, on_signature_help(params));
            if (method == "textDocument/documentSymbol")
                return m_transport.respond(id, on_document_symbol(params));
            if (method == "workspace/symbol")
                return m_transport.respond(id, on_workspace_symbol(params));
            if (method == "sclang/status")
                return m_transport.respond(id, on_status());
            m_transport.respond_error(id, rpc::error_code::method_not_found, "Unknown method " + method);
        } catch (const std::exception& e) {
            log("error handling " + method + ": " + e.what());
            if (is_request)
                m_transport.respond_error(id, rpc::error_code::internal_error, e.what());
        }
    }

    Json initialize(const Json& params) {
        m_initialized = true;
        const auto& init = params["initializationOptions"];
        if (!m_options.library_given) {
            for (const auto& p : init["libraryPaths"].items())
                m_options.library.modules.push_back(std::filesystem::u8path(p.str()));
            if (m_options.library.modules.empty())
                m_options.library.modules = default_modules(m_executable);
        }
        for (const auto& p : init["excludePaths"].items())
            m_options.library.excludes.push_back(std::filesystem::u8path(p.str()));
        if (init["interpretedVersion"].is_string())
            if (const auto v = sc::ast::parse_release(init["interpretedVersion"].str()))
                m_options.library.interpreted_version = *v;
        if (init["sclangPort"].is_number())
            m_options.hooks.port = static_cast<std::uint16_t>(init["sclangPort"].integer());
        if (init["hooks"].is_bool())
            m_options.hooks_enabled = init["hooks"].boolean();
        m_hooks = make_hook_host(m_options.hooks);

        for (const auto& encoding : params["capabilities"]["general"]["positionEncodings"].items())
            if (encoding.str() == "utf-32")
                m_utf32 = true;

        std::string modules;
        for (const auto& m : m_options.library.modules)
            modules += "\n  " + m.u8string();
        log("class library modules:" + (modules.empty() ? std::string { " none" } : modules));
        schedule_rebuild(Clock::now());

        Json sync = Json::object();
        sync["openClose"] = true;
        sync["change"] = 1; // full text
        Json save = Json::object();
        save["includeText"] = false;
        sync["save"] = save;

        Json caps = Json::object();
        caps["positionEncoding"] = m_utf32 ? "utf-32" : "utf-16";
        caps["textDocumentSync"] = sync;
        caps["hoverProvider"] = true;
        caps["definitionProvider"] = true;
        caps["referencesProvider"] = true;
        caps["documentSymbolProvider"] = true;
        caps["workspaceSymbolProvider"] = true;
        Json completion = Json::object();
        completion["triggerCharacters"] = Json { Json::Array { ".", "\\", "~", "'" } };
        completion["resolveProvider"] = false;
        caps["completionProvider"] = completion;
        Json signature = Json::object();
        signature["triggerCharacters"] = Json { Json::Array { "(", "," } };
        caps["signatureHelpProvider"] = signature;

        Json info = Json::object();
        info["name"] = "sc_lsp";
        info["version"] = version;
        Json result = Json::object();
        result["capabilities"] = caps;
        result["serverInfo"] = info;
        return result;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Documents
    ////////////////////////////////////////////////////////////////////////////////

    struct Open {
        std::unique_ptr<Document> document;
        std::int64_t version { 0 };
    };

    Open* find(const Json& params) {
        const auto fnd = m_documents.find(params["textDocument"]["uri"].str());
        return fnd == m_documents.end() ? nullptr : &fnd->second;
    }

    void did_open(const Json& params) {
        const auto& doc = params["textDocument"];
        const auto uri = doc["uri"].str();
        auto path = uri_to_path(uri);
        if (!path.empty())
            path = normal_path(path);
        const auto kind = path.empty()
            ? (doc["languageId"].str() == "supercollider-class" ? DocumentKind::ClassFile : DocumentKind::Interpreted)
            : kind_of(path);
        auto& open = m_documents[uri];
        open.document = std::make_unique<Document>(path, doc["text"].str(), kind);
        open.version = doc["version"].integer();
        analyse_now(uri);
        if (kind == DocumentKind::ClassFile && !in_sync(*open.document))
            schedule_rebuild(Clock::now() + rebuild_delay);
    }

    void did_change(const Json& params) {
        auto* open = find(params);
        if (!open)
            return;
        // Full sync: the last change holds the whole text.
        const auto& changes = params["contentChanges"].items();
        if (changes.empty())
            return;
        open->document->set_text(changes.back()["text"].str());
        open->version = params["textDocument"]["version"].integer(open->version + 1);
        const auto uri = params["textDocument"]["uri"].str();
        m_pending.insert(uri);
        m_analysis_deadline = Clock::now() + analysis_delay;
        if (open->document->kind() == DocumentKind::ClassFile)
            schedule_rebuild(Clock::now() + rebuild_delay);
    }

    void did_save(const Json& params) {
        if (auto* open = find(params); open && open->document->kind() == DocumentKind::ClassFile)
            schedule_rebuild(Clock::now());
    }

    void did_close(const Json& params) {
        const auto uri = params["textDocument"]["uri"].str();
        const auto fnd = m_documents.find(uri);
        if (fnd == m_documents.end())
            return;
        const bool class_file = fnd->second.document->kind() == DocumentKind::ClassFile;
        const bool unsaved = class_file && !in_sync_with_disk(*fnd->second.document);
        m_documents.erase(fnd);
        m_pending.erase(uri);
        Json p = Json::object();
        p["uri"] = uri;
        p["diagnostics"] = Json::array();
        m_transport.notify("textDocument/publishDiagnostics", p);
        if (unsaved)
            schedule_rebuild(Clock::now());
    }

    // Whether the library was compiled from the document's text.
    bool in_sync(const Document& d) const {
        if (!m_library || d.path().empty())
            return false;
        const auto text = m_library->text_of(d.path());
        return text && text->source.as_string() == sc::src::NormalisedSource { d.text() }.as_string();
    }

    bool in_sync_with_disk(const Document& d) const {
        for (const auto& f : m_library ? m_library->files() : std::vector<FileStamp> {})
            if (f.path == d.path())
                return !f.overlaid;
        return true;
    }

    // Analyses a document now if its text changed, e.g., before answering a request about it.
    Document* fresh(const Json& params) {
        auto* open = find(params);
        if (!open)
            return nullptr;
        const auto uri = params["textDocument"]["uri"].str();
        if (open->document->stale() || m_pending.count(uri))
            analyse_now(uri);
        return open->document.get();
    }

    void analyse_now(const std::string& uri) {
        m_pending.erase(uri);
        const auto fnd = m_documents.find(uri);
        if (fnd == m_documents.end())
            return;
        auto& doc = *fnd->second.document;
        doc.analyse(m_library);
        publish(uri, fnd->second);
    }

    void publish(const std::string& uri, const Open& open) {
        const auto& doc = *open.document;
        Json list = Json::array();
        for (const auto& d : all_diagnostics(doc)) {
            Json item = Json::object();
            item["range"] = range_json(doc, d.range);
            item["severity"] = d.severity == Severity::Error ? 1 : d.severity == Severity::Warning ? 2 : 3;
            item["source"] = "sclang";
            item["code"] = d.name;
            item["message"] = d.message;
            Json related = Json::array();
            for (const auto& [where, text] : d.related) {
                Json r = Json::object();
                r["location"] = location_json(doc, uri, where);
                r["message"] = text.empty() ? std::string { "here" } : text;
                related.push_back(std::move(r));
            }
            if (related.size())
                item["relatedInformation"] = std::move(related);
            list.push_back(std::move(item));
        }
        Json p = Json::object();
        p["uri"] = uri;
        p["version"] = open.version;
        p["diagnostics"] = std::move(list);
        m_transport.notify("textDocument/publishDiagnostics", p);
    }

    ////////////////////////////////////////////////////////////////////////////////
    // The library
    ////////////////////////////////////////////////////////////////////////////////

    void schedule_rebuild(Clock::time_point when) {
        if (!m_rebuild_deadline || when < *m_rebuild_deadline)
            m_rebuild_deadline = when;
    }

    void start_rebuild() {
        m_rebuild_deadline.reset();
        if (m_rebuild.valid()) {
            m_rebuild_again = true;
            return;
        }
        Overlays overlays;
        for (const auto& [uri, open] : m_documents)
            if (open.document->kind() == DocumentKind::ClassFile && !open.document->path().empty())
                overlays[open.document->path()] = open.document->text();
        auto config = m_options.library;
        m_rebuild = std::async(std::launch::async,
                               [config, overlays = std::move(overlays)] { return build_library(config, overlays); });
    }

    void finish_rebuild() {
        m_library = m_rebuild.get();
        m_last_disk_check = Clock::now();
        Json p = Json::object();
        p["type"] = 3;
        p["message"] = "sc_lsp: compiled " + std::to_string(m_library->classes().size()) + " classes from "
            + std::to_string(m_library->files().size()) + " files in "
            + std::to_string(static_cast<int>(m_library->build_seconds() * 1000)) + " ms, "
            + std::to_string(m_library->diagnostics().size()) + " diagnostics";
        log(p["message"].str());
        if (!m_library->log().empty())
            log(m_library->log());
        m_transport.notify("window/logMessage", p);
        if (m_hooks && m_options.hooks_enabled && !m_library->hooks().raw().empty())
            log(std::to_string(m_library->hooks().size()) + " hooks, " + m_hooks->status());
        for (const auto& [uri, open] : m_documents)
            m_pending.insert(uri);
        m_analysis_deadline = Clock::now();
        if (m_rebuild_again) {
            m_rebuild_again = false;
            schedule_rebuild(Clock::now());
        }
    }

    std::optional<Clock::time_point> next_deadline() const {
        std::optional<Clock::time_point> out;
        for (const auto& d : { m_analysis_deadline, m_rebuild_deadline })
            if (d && (!out || *d < *out))
                out = d;
        if (m_rebuild.valid()) {
            const auto soon = Clock::now() + std::chrono::milliseconds { 50 };
            if (!out || soon < *out)
                out = soon;
        }
        return out;
    }

    void timers() {
        const auto now = Clock::now();
        if (m_rebuild.valid() && m_rebuild.wait_for(std::chrono::seconds { 0 }) == std::future_status::ready)
            finish_rebuild();
        if (m_rebuild_deadline && *m_rebuild_deadline <= now)
            start_rebuild();
        if (m_analysis_deadline && *m_analysis_deadline <= now) {
            m_analysis_deadline.reset();
            const auto pending = m_pending;
            for (const auto& uri : pending)
                analyse_now(uri);
        }
        if (m_library && !m_rebuild.valid() && now - m_last_disk_check > disk_check_interval) {
            m_last_disk_check = now;
            if (m_library->files_changed())
                schedule_rebuild(now);
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Positions
    ////////////////////////////////////////////////////////////////////////////////

    Position position(const Document& doc, const Json& p) const {
        const auto line = static_cast<std::uint32_t>(p["line"].integer());
        const auto character = static_cast<std::uint32_t>(p["character"].integer());
        return { line, m_utf32 ? character : doc.lines().utf16_to_codepoint(line, character) };
    }

    Json position_json(const LineIndex& lines, Position p) const {
        Json out = Json::object();
        out["line"] = p.line;
        out["character"] = m_utf32 ? p.column : lines.codepoint_to_utf16(p.line, p.column);
        return out;
    }

    Json range_json(const LineIndex& lines, Range r) const {
        Json out = Json::object();
        out["start"] = position_json(lines, r.begin);
        out["end"] = position_json(lines, r.end);
        return out;
    }
    Json range_json(const Document& doc, Range r) const { return range_json(doc.lines(), r); }

    // A location without a path is in the document. Another file's columns are converted with its text.
    Json location_json(const Document& doc, const std::string& uri, const Location& l) {
        Json out = Json::object();
        if (l.path.empty() || l.path == doc.path()) {
            out["uri"] = uri;
            out["range"] = range_json(doc, l.range);
            return out;
        }
        const auto file_uri = path_to_uri(l.path);
        out["uri"] = file_uri;
        if (const auto fnd = m_documents.find(file_uri); fnd != m_documents.end()) {
            out["range"] = range_json(*fnd->second.document, l.range);
        } else if (const auto text = m_library ? m_library->text_of(l.path) : nullptr) {
            const LineIndex lines { std::string_view { text->source.as_string() } };
            out["range"] = range_json(lines, l.range);
        } else {
            out["range"] = range_json(LineIndex {}, l.range);
        }
        return out;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Requests
    ////////////////////////////////////////////////////////////////////////////////

    Json on_hover(const Json& params) {
        auto* doc = fresh(params);
        if (!doc)
            return {};
        const auto h = hover(*doc, position(*doc, params["position"]));
        if (!h)
            return {};
        Json contents = Json::object();
        contents["kind"] = "markdown";
        contents["value"] = h->markdown;
        Json out = Json::object();
        out["contents"] = contents;
        out["range"] = range_json(*doc, h->range);
        return out;
    }

    Json on_definition(const Json& params) {
        auto* doc = fresh(params);
        Json out = Json::array();
        if (!doc)
            return out;
        const auto uri = params["textDocument"]["uri"].str();
        for (const auto& l : definition(*doc, position(*doc, params["position"])))
            out.push_back(location_json(*doc, uri, l));
        return out;
    }

    Json on_references(const Json& params) {
        auto* doc = fresh(params);
        Json out = Json::array();
        if (!doc)
            return out;
        const auto uri = params["textDocument"]["uri"].str();
        std::vector<const Document*> others;
        for (const auto& [_, open] : m_documents)
            others.push_back(open.document.get());
        const bool declaration = params["context"]["includeDeclaration"].boolean(true);
        for (const auto& l : references(*doc, position(*doc, params["position"]), declaration, others))
            out.push_back(location_json(*doc, uri, l));
        return out;
    }

    static int completion_kind(CompletionKind k) {
        switch (k) {
        case CompletionKind::Class:
            return 7;
        case CompletionKind::Method:
            return 2;
        case CompletionKind::Variable:
            return 6;
        case CompletionKind::Member:
            return 5;
        case CompletionKind::Keyword:
            return 14;
        case CompletionKind::Argument:
            return 10;
        case CompletionKind::Symbol:
            return 20;
        case CompletionKind::Environment:
            return 6;
        }
        return 1;
    }

    Json on_completion(const Json& params) {
        auto* doc = fresh(params);
        Json out = Json::object();
        out["isIncomplete"] = false;
        out["items"] = Json::array();
        if (!doc)
            return out;
        HookHost* hooks = m_options.hooks_enabled ? m_hooks.get() : nullptr;
        const auto result = complete(*doc, position(*doc, params["position"]), hooks);
        Json items = Json::array();
        for (const auto& c : result.items) {
            Json item = Json::object();
            item["label"] = c.label;
            item["kind"] = completion_kind(c.kind);
            if (!c.detail.empty())
                item["detail"] = c.detail;
            if (!c.documentation.empty())
                item["documentation"] = c.documentation;
            item["sortText"] = std::to_string(c.rank) + c.label;
            if (!c.filter.empty())
                item["filterText"] = c.filter;
            Json edit = Json::object();
            edit["range"] = range_json(*doc, c.replace ? *c.replace : result.replace);
            edit["newText"] = c.insert.empty() ? c.label : c.insert;
            item["textEdit"] = std::move(edit);
            items.push_back(std::move(item));
        }
        out["isIncomplete"] = result.incomplete;
        out["items"] = std::move(items);
        return out;
    }

    Json on_signature_help(const Json& params) {
        auto* doc = fresh(params);
        if (!doc)
            return {};
        const auto help = signature_help(*doc, position(*doc, params["position"]));
        if (!help)
            return {};
        Json signatures = Json::array();
        for (const auto& s : help->signatures) {
            Json sig = Json::object();
            sig["label"] = s.label;
            sig["documentation"] = s.documentation;
            Json parameters = Json::array();
            for (const auto& [b, e] : s.parameters) {
                Json p = Json::object();
                p["label"] = Json { Json::Array { Json { b }, Json { e } } };
                parameters.push_back(std::move(p));
            }
            sig["parameters"] = std::move(parameters);
            signatures.push_back(std::move(sig));
        }
        Json out = Json::object();
        out["signatures"] = std::move(signatures);
        out["activeSignature"] = help->active_signature;
        out["activeParameter"] = help->active_parameter;
        return out;
    }

    static int symbol_kind(SymbolInfo::Kind k) {
        switch (k) {
        case SymbolInfo::Kind::Class:
            return 5;
        case SymbolInfo::Kind::Method:
            return 6;
        case SymbolInfo::Kind::ClassMethod:
            return 12;
        case SymbolInfo::Kind::Variable:
            return 13;
        case SymbolInfo::Kind::ClassVariable:
            return 8;
        case SymbolInfo::Kind::Constant:
            return 14;
        }
        return 13;
    }

    Json document_symbol_json(const Document& doc, const SymbolInfo& s) const {
        Json out = Json::object();
        out["name"] = s.name;
        if (!s.detail.empty())
            out["detail"] = s.detail;
        out["kind"] = symbol_kind(s.kind);
        out["range"] = range_json(doc, s.location.range);
        out["selectionRange"] = range_json(doc, s.selection);
        Json children = Json::array();
        for (const auto& c : s.children)
            children.push_back(document_symbol_json(doc, c));
        out["children"] = std::move(children);
        return out;
    }

    Json on_document_symbol(const Json& params) {
        auto* doc = fresh(params);
        Json out = Json::array();
        if (!doc)
            return out;
        for (const auto& s : document_symbols(*doc))
            out.push_back(document_symbol_json(*doc, s));
        return out;
    }

    Json on_workspace_symbol(const Json& params) {
        Json out = Json::array();
        if (!m_library)
            return out;
        for (const auto& s : workspace_symbols(*m_library, params["query"].str())) {
            Json item = Json::object();
            item["name"] = s.name;
            item["kind"] = symbol_kind(s.kind);
            item["containerName"] = s.detail;
            Json location = Json::object();
            location["uri"] = path_to_uri(s.location.path);
            const auto text = m_library->text_of(s.location.path);
            location["range"] = range_json(
                text ? LineIndex { std::string_view { text->source.as_string() } } : LineIndex {}, s.location.range);
            item["location"] = std::move(location);
            out.push_back(std::move(item));
        }
        return out;
    }

    Json on_status() {
        Json out = Json::object();
        out["libraryReady"] = m_library != nullptr;
        if (m_library) {
            out["classes"] = m_library->classes().size();
            out["files"] = m_library->files().size();
            out["hooks"] = m_library->hooks().size();
            out["buildSeconds"] = m_library->build_seconds();
        }
        out["hooksSupported"] = hooks_supported();
        out["sclang"] = m_hooks ? m_hooks->status() : std::string { "hooks off" };
        return out;
    }

    rpc::Transport& m_transport;
    Options m_options;
    std::filesystem::path m_executable;
    std::unique_ptr<HookHost> m_hooks;
    bool m_utf32 { false };
    bool m_initialized { false };
    bool m_shutdown { false };
    bool m_exit { false };

    std::mutex m_lock;
    std::condition_variable m_wake;
    std::deque<Json> m_inbox;
    bool m_input_closed { false };

    std::map<std::string, Open> m_documents;
    std::set<std::string> m_pending;
    std::optional<Clock::time_point> m_analysis_deadline;

    std::shared_ptr<Library> m_library;
    std::future<std::shared_ptr<Library>> m_rebuild;
    std::optional<Clock::time_point> m_rebuild_deadline;
    bool m_rebuild_again { false };
    Clock::time_point m_last_disk_check { Clock::now() };
};

int usage() {
    std::cerr << "usage: sc_lsp [--library PATH]... [--exclude PATH]... [--interpreted-version 3.16|legacy]\n"
                 "              [--sclang-port N] [--no-hooks] [--threads N]\n"
                 "A language server for SuperCollider, over stdin and stdout.\n";
    return EXIT_FAILURE;
}

}

int main(int argc, char* argv[]) {
    Options options;
    for (int i = 1; i < argc; ++i) {
        const std::string_view arg { argv[i] };
        const bool has_value = i + 1 < argc;
        if (arg == "--library" && has_value) {
            options.library.modules.push_back(std::filesystem::u8path(argv[++i]));
            options.library_given = true;
        } else if (arg == "--exclude" && has_value) {
            options.library.excludes.push_back(std::filesystem::u8path(argv[++i]));
        } else if (arg == "--interpreted-version" && has_value) {
            const auto v = sc::ast::parse_release(argv[++i]);
            if (!v)
                return usage();
            options.library.interpreted_version = *v;
        } else if (arg == "--sclang-port" && has_value) {
            options.hooks.port = static_cast<std::uint16_t>(std::stoi(argv[++i]));
        } else if (arg == "--threads" && has_value) {
            options.library.num_threads = static_cast<std::size_t>(std::stoul(argv[++i]));
        } else if (arg == "--no-hooks") {
            options.hooks_enabled = false;
        } else if (arg == "--stdio") {
            // Some clients always pass it.
        } else if (arg == "--version") {
            std::cout << "sc_lsp " << version << "\n";
            return 0;
        } else {
            return usage();
        }
    }
    rpc::use_binary_stdio();
    std::ios::sync_with_stdio(false);
    // Reading cin would flush cout, from the reader thread, racing the writes.
    std::cin.tie(nullptr);
    rpc::Transport transport { std::cin, std::cout, rpc::Framing::ContentLength };
    Server server { transport, std::move(options), executable_path(argv[0]) };
    return server.run();
}
