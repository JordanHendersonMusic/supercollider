// sc_mcp: an MCP server for SuperCollider, over stdio, for coding agents.
//   sc_mcp [--library PATH]... [--exclude PATH]... [--interpreted-version 3.16|legacy] [--sclang-port N]
//          [--no-hooks] [--allow-eval]
// The tools check code and look things up in the class library, which it reads from disk and rebuilds when the files
// change. `evaluate` runs code in the user's sclang, so it is only listed with --allow-eval, in a build with
// SC_LSP_CONNECTED, and sclang must allow it too (LSPHook.allowEval = true).

#include "sc_query/document.hpp"
#include "sc_query/hooks.hpp"
#include "sc_query/jsonrpc.hpp"
#include "sc_query/library.hpp"
#include "sc_query/queries.hpp"

#include <chrono>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <sstream>

namespace {

using namespace sc::query;
using Clock = std::chrono::steady_clock;

constexpr const char* version = "0.1.0";
constexpr const char* default_protocol = "2025-06-18";

struct Options {
    LibraryConfig library;
    bool library_given { false };
    HookHostConfig hooks;
    bool hooks_enabled { true };
};

Json string_property(const char* description) {
    Json p = Json::object();
    p["type"] = "string";
    p["description"] = description;
    return p;
}

Json schema(std::vector<std::pair<const char*, Json>> properties, std::vector<std::string> required) {
    Json s = Json::object();
    s["type"] = "object";
    Json props = Json::object();
    for (auto& [name, p] : properties)
        props[name] = std::move(p);
    s["properties"] = std::move(props);
    Json req = Json::array();
    for (auto& r : required)
        req.push_back(std::move(r));
    s["required"] = std::move(req);
    return s;
}

class Server {
public:
    Server(rpc::Transport& transport, Options options, std::filesystem::path executable):
        m_transport(transport),
        m_options(std::move(options)),
        m_executable(std::move(executable)) {
        if (!m_options.library_given)
            m_options.library.modules = default_modules(m_executable);
        m_hooks = make_hook_host(m_options.hooks);
        define_tools();
    }

    int run() {
        while (auto message = m_transport.read())
            handle(*message);
        return 0;
    }

private:
    struct Tool {
        std::string description;
        Json input_schema;
        std::function<std::string(const Json&, bool&)> call; // sets the bool on an error
    };

    void handle(const Json& message) {
        const auto method = message["method"].str();
        const auto& id = message["id"];
        if (method.empty() || !message.contains("id"))
            return; // notifications and responses
        try {
            if (method == "initialize") {
                Json result = Json::object();
                const auto asked = message["params"]["protocolVersion"].str();
                result["protocolVersion"] = asked.empty() ? default_protocol : asked;
                Json caps = Json::object();
                Json tools = Json::object();
                tools["listChanged"] = false;
                caps["tools"] = tools;
                result["capabilities"] = caps;
                Json info = Json::object();
                info["name"] = "sc_mcp";
                info["version"] = version;
                result["serverInfo"] = info;
                result["instructions"] =
                    "Tools for SuperCollider code. check_code compiles sclang code (interpreted code, as in an .scd "
                    "file, or a class file) against the class library and reports errors with line:column. The "
                    "lookups (class_info, implementors, find_definition, subclasses, search, references) read the "
                    "class library from disk and answer with file:line, so the source can be read directly.";
                return m_transport.respond(id, result);
            }
            if (method == "ping")
                return m_transport.respond(id, Json::object());
            if (method == "tools/list") {
                Json list = Json::array();
                for (const auto& [name, tool] : m_tools) {
                    Json t = Json::object();
                    t["name"] = name;
                    t["description"] = tool.description;
                    t["inputSchema"] = tool.input_schema;
                    list.push_back(std::move(t));
                }
                Json result = Json::object();
                result["tools"] = std::move(list);
                return m_transport.respond(id, result);
            }
            if (method == "tools/call") {
                const auto& params = message["params"];
                const auto fnd = m_tools.find(params["name"].str());
                if (fnd == m_tools.end())
                    return m_transport.respond_error(id, rpc::error_code::invalid_params,
                                                     "Unknown tool " + params["name"].str());
                bool error = false;
                const auto text = fnd->second.call(params["arguments"], error);
                Json content = Json::object();
                content["type"] = "text";
                content["text"] = text;
                Json result = Json::object();
                result["content"] = Json { Json::Array { content } };
                result["isError"] = error;
                return m_transport.respond(id, result);
            }
            m_transport.respond_error(id, rpc::error_code::method_not_found, "Unknown method " + method);
        } catch (const std::exception& e) { m_transport.respond_error(id, rpc::error_code::internal_error, e.what()); }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // The library, rebuilt when its files change
    ////////////////////////////////////////////////////////////////////////////////

    Library& library() {
        const auto now = Clock::now();
        if (!m_library || (now - m_checked > std::chrono::seconds { 2 } && m_library->files_changed())) {
            m_library = build_library(m_options.library);
            std::cerr << "[sc_mcp] compiled " << m_library->classes().size() << " classes in "
                      << static_cast<int>(m_library->build_seconds() * 1000) << " ms" << std::endl;
        }
        m_checked = now;
        return *m_library;
    }

    std::string check(const std::string& code, DocumentKind kind, const std::filesystem::path& path) {
        auto& lib = library();
        const auto name = path.empty() ? std::string { kind == DocumentKind::ClassFile ? "snippet.sc" : "snippet.scd" }
                                       : path.u8string();
        if (kind == DocumentKind::Interpreted) {
            Document doc { path, code, kind };
            doc.analyse(m_library);
            return describe_diagnostics(all_diagnostics(doc), name);
        }
        // A class file is compiled with the library: its own file replaced, or as one more file.
        const auto normal = path.empty() ? normal_path("snippet.sc") : normal_path(path);
        std::shared_ptr<Library> with;
        if (lib.text_of(normal)) {
            Overlays overlays;
            overlays[normal] = code;
            with = build_library(m_options.library, overlays);
        } else {
            with = build_library(m_options.library, {}, { { normal, code } });
        }
        Document doc { normal, code, kind };
        doc.analyse(with);
        return describe_diagnostics(all_diagnostics(doc), name);
    }

    static std::string read_file(const std::filesystem::path& path, bool& error) {
        std::ifstream in { path, std::ios::binary };
        if (!in.is_open()) {
            error = true;
            return {};
        }
        std::stringstream ss;
        ss << in.rdbuf();
        return std::move(ss).str();
    }

    void define_tools() {
        Json kind = string_property("`interpreted` (the default), as in an .scd file, or `class` for a class file.");
        m_tools["check_code"] = {
            "Compiles SuperCollider code against the class library without running it, and lists the errors and "
            "warnings with line:column. Interpreted code is compiled as the IDE evaluates an .scd file's regions.",
            schema({ { "code", string_property("The code.") }, { "kind", kind } }, { "code" }),
            [this](const Json& a, bool&) {
                const auto k = a["kind"].str() == "class" ? DocumentKind::ClassFile : DocumentKind::Interpreted;
                return check(a["code"].str(), k, {});
            }
        };
        m_tools["check_file"] = {
            "Compiles a .sc or .scd file from disk and lists its errors and warnings. A .sc file is compiled with "
            "the class library, replacing its copy if it is part of it.",
            schema({ { "path", string_property("The file's path.") } }, { "path" }),
            [this](const Json& a, bool& error) {
                const auto path = normal_path(std::filesystem::u8path(a["path"].str()));
                const auto text = read_file(path, error);
                if (error)
                    return "Could not read " + path.u8string();
                return check(text, kind_of(path), path);
            }
        };
        m_tools["class_info"] = {
            "A class: its superclasses, subclasses, variables, and class and instance methods with their arguments, "
            "each with file:line. Set inherited to list inherited methods too.",
            schema({ { "name", string_property("The class name, e.g. SinOsc.") },
                     { "inherited",
                       [] {
                           Json p = Json::object();
                           p["type"] = "boolean";
                           p["description"] = "Include inherited methods.";
                           return p;
                       }() } },
                   { "name" }),
            [this](const Json& a, bool&) {
                return describe_class(library(), a["name"].str(), a["inherited"].boolean());
            }
        };
        m_tools["implementors"] = {
            "Every class that implements a method, with its arguments and file:line.",
            schema({ { "selector", string_property("The method name, e.g. ar or play.") } }, { "selector" }),
            [this](const Json& a, bool&) { return describe_implementors(library(), a["selector"].str()); }
        };
        m_tools["find_definition"] = {
            "Where something is defined: a class (Foo), a method of a class as dispatched (Foo:bar for an instance "
            "method, Foo.*bar or Foo.bar for a class method), or a bare selector.",
            schema({ { "name", string_property("e.g. Pbind, SinOsc.*ar, Array:collect, or collect.") } }, { "name" }),
            [this](const Json& a, bool&) { return describe_definition(library(), a["name"].str()); }
        };
        m_tools["subclasses"] = { "The class tree under a class.",
                                  schema({ { "name", string_property("The class name, e.g. UGen.") } }, { "name" }),
                                  [this](const Json& a, bool&) {
                                      return describe_subclasses(library(), a["name"].str());
                                  } };
        m_tools["search"] = { "Classes and methods whose names contain the text, ignoring case.",
                              schema({ { "query", string_property("Part of a class or method name.") } }, { "query" }),
                              [this](const Json& a, bool&) { return describe_search(library(), a["query"].str()); } };
        m_tools["references"] = {
            "Every place in the class library that sends a selector, or uses a class (a capitalised name).",
            schema({ { "name", string_property("A selector, e.g. play, or a class, e.g. Pbind.") } }, { "name" }),
            [this](const Json& a, bool&) { return describe_references(library(), a["name"].str()); }
        };
        m_tools["hooks"] = { "The language server hooks (subclasses of LSPHook) in the class library, and whether "
                             "the running sclang can run them.",
                             schema({}, {}), [this](const Json&, bool&) { return describe_hooks(); } };

        if (!hooks_supported() || !m_options.hooks_enabled)
            return;
        m_tools["complete_symbol"] = {
            "Asks the running sclang's hooks which symbols fit somewhere, e.g. the SynthDef names valid for "
            "\\instrument in a Pbind (receiver Pbind, selector new, previous \\instrument). Needs sclang running "
            "with the class library.",
            schema({ { "receiver", string_property("The class the call is sent to, e.g. Pbind or Synth.") },
                     { "selector", string_property("The call's selector, e.g. new.") },
                     { "previous", string_property("The argument before, e.g. \\instrument.") },
                     { "keyword", string_property("The keyword when written key: \\..., e.g. instrument.") },
                     { "prefix", string_property("What has been typed of the symbol so far.") } },
                   {}),
            [this](const Json& a, bool& error) {
                SymbolContext context;
                context.receiver = a["receiver"].str();
                context.selector = a["selector"].str();
                context.previous = a["previous"].str();
                context.keyword = a["keyword"].str();
                context.prefix = a["prefix"].str();
                context.argument = context.previous.empty() ? 0 : 1;
                if (!m_hooks->connected()) {
                    error = true;
                    return m_hooks->status();
                }
                std::string out;
                for (const auto& c : m_hooks->complete_symbol(library(), context))
                    if (c.label.rfind(context.prefix, 0) == 0)
                        out += "\\" + c.label + (c.detail.empty() ? "" : "    " + c.detail) + "\n";
                return out.empty() ? std::string { "No hook had a suggestion.\n" } : out;
            }
        };
        if (!m_options.hooks.allow_eval)
            return;
        m_tools["evaluate"] = {
            "Evaluates code in the user's running sclang and returns the result's compile string or the error. The "
            "code is posted in sclang's post window first. It can make sound and change the running state.",
            schema({ { "code", string_property("The code, as if evaluated in the IDE.") } }, { "code" }),
            [this](const Json& a, bool& error) {
                const auto r = m_hooks->evaluate(a["code"].str());
                error = !r.ok;
                return r.text;
            }
        };
    }

    std::string describe_hooks() {
        auto& lib = library();
        std::ostringstream out;
        if (!hooks_supported())
            out << "This build runs no hooks (built without SC_LSP_CONNECTED).\n";
        else
            out << (m_options.hooks_enabled ? m_hooks->status() : std::string { "hooks are off (--no-hooks)" }) << "\n";
        if (lib.hooks().raw().empty())
            out << "No class derives from " << hook_base_class << ".\n";
        for (const auto id : lib.hooks().indices()) {
            const auto& h = lib.hooks()[id];
            out << "\n"
                << lib.get(h.hook_class).name << "    " << Library::where(lib.get(h.hook_class).location) << "\n";
            for (const auto& s : h.selectors)
                out << "  *" << s << "\n";
            if (hooks_supported() && m_options.hooks_enabled) {
                const auto stale = m_hooks->stale_reason(lib, id);
                out << "  " << (stale.empty() ? std::string { "runs in sclang" } : "can't run: " + stale) << "\n";
            }
        }
        return out.str();
    }

    rpc::Transport& m_transport;
    Options m_options;
    std::filesystem::path m_executable;
    std::unique_ptr<HookHost> m_hooks;
    std::shared_ptr<Library> m_library;
    Clock::time_point m_checked { Clock::now() };
    std::map<std::string, Tool> m_tools;
};

int usage() {
    std::cerr << "usage: sc_mcp [--library PATH]... [--exclude PATH]... [--interpreted-version 3.16|legacy]\n"
                 "              [--sclang-port N] [--no-hooks] [--allow-eval]\n"
                 "An MCP server for SuperCollider, over stdin and stdout.\n"
                 "--allow-eval lists the evaluate tool, which runs code in the running sclang. sclang must allow it\n"
                 "too, with LSPHook.allowEval = true.\n";
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
        } else if (arg == "--no-hooks") {
            options.hooks_enabled = false;
        } else if (arg == "--allow-eval") {
            options.hooks.allow_eval = true;
        } else if (arg == "--version") {
            std::cout << "sc_mcp " << version << "\n";
            return 0;
        } else {
            return usage();
        }
    }
    rpc::use_binary_stdio();
    std::ios::sync_with_stdio(false);
    // Reading cin would flush cout, from the reader thread, racing the writes.
    std::cin.tie(nullptr);
    rpc::Transport transport { std::cin, std::cout, rpc::Framing::NewlineDelimited };
    Server server { transport, std::move(options), executable_path(argv[0]) };
    return server.run();
}
