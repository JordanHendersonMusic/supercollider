#include "sc_query/document.hpp"

#include "sc_compiler/sc_compiler.hpp"
#include "sc_diagnostic/sc_diagnostic.hpp"
#include "sc_parser/ast.hpp"
#include "sc_parser/sc_parser.hpp"
#include "sc_sema/ir.hpp"
#include "sc_source/normalise_source.hpp"
#include "sc_source/text_info.hpp"

namespace sc::query {

DocumentKind kind_of(const std::filesystem::path& path) {
    return path.extension() == ".sc" ? DocumentKind::ClassFile : DocumentKind::Interpreted;
}

Document::Document(std::filesystem::path path, std::string text, DocumentKind kind):
    m_path(std::move(path)),
    m_text(std::move(text)),
    m_lines(m_text),
    m_kind(kind) {}

Document::~Document() = default;

void Document::set_text(std::string text) {
    m_text = std::move(text);
    m_lines = LineIndex { m_text };
    m_stale = true;
}

DiagnosticInfo to_info(const diag::Diagnostic& d, const src::TextInfo* text) {
    DiagnosticInfo info;
    info.severity = d.severity() == diag::Severity::Error ? Severity::Error : Severity::Warning;
    info.name = d.name();
    bool placed = false;
    for (const auto& l : d.locations()) {
        const bool here = l.text_info.get() == text
            || (text && text->file_path && l.text_info && l.text_info->file_path
                && *l.text_info->file_path == *text->file_path);
        if (here && !placed) {
            info.range = to_range(l.range);
            info.message = l.msg;
            // `this is not defined.` refers to the highlighted code, which the editor may not show beside it.
            const auto code = l.text_info ? std::string { l.text_info->read(l.range) } : std::string {};
            if (info.message.rfind("this ", 0) == 0 && !code.empty() && code.size() < 60
                && code.find('\n') == std::string::npos)
                info.message = "`" + code + "`" + info.message.substr(4);
            placed = true;
            continue;
        }
        Location where;
        if (!here)
            where.path = l.text_info && l.text_info->file_path ? *l.text_info->file_path : std::filesystem::path {};
        where.range = to_range(l.range);
        info.related.emplace_back(std::move(where), l.msg);
    }
    if (!placed && !d.locations().empty())
        info.message = d.locations().front().msg;
    if (!d.extra().empty())
        info.message += (info.message.empty() ? "" : "\n") + d.extra();
    if (info.message.empty())
        info.message = info.name;
    return info;
}

namespace {
bool in_file(const diag::Diagnostic& d, const src::TextInfo* text, const std::filesystem::path& path) {
    for (const auto& l : d.locations()) {
        if (l.text_info.get() == text)
            return true;
        if (!path.empty() && l.text_info && l.text_info->file_path && *l.text_info->file_path == path)
            return true;
    }
    return false;
}
}

void Document::analyse(const std::shared_ptr<Library>& library) {
    m_stale = false;
    m_library = library;
    m_diagnostics.clear();
    m_ast.reset();
    const auto path = m_path.empty() ? std::optional<std::filesystem::path> {} : std::optional { m_path };
    try {
        m_text_info = std::make_shared<const src::TextInfo>(
            src::NormalisedSource { m_text }, src::FileCodeLocation {}, path,
            m_kind == DocumentKind::ClassFile ? src::TextKind::ClassFile : src::TextKind::ScdFile);
    } catch (const std::length_error&) {
        m_text_info.reset();
        DiagnosticInfo too_large;
        too_large.name = "File too large.";
        too_large.message = "The file is too large to compile.";
        m_diagnostics.push_back(std::move(too_large));
        return;
    }

    if (m_kind == DocumentKind::ClassFile) {
        // The library's compile of this file is used when it has this text, otherwise only the parse.
        const auto in_library = library ? library->text_of(m_path) : nullptr;
        if (in_library && in_library->source.as_string() == m_text_info->source.as_string()) {
            for (const auto& d : library->diagnostics())
                if (in_file(d, in_library.get(), m_path))
                    m_diagnostics.push_back(to_info(d, in_library.get()));
            return;
        }
        auto [ast, accepted] = ast::parse(m_text_info);
        (void)accepted;
        m_ast = ast;
        if (ast)
            for (const auto& d : ast->diagnostics())
                m_diagnostics.push_back(to_info(d, m_text_info.get()));
        return;
    }

    auto parsed = compiler::parse_interpreted(m_text_info);
    bool errors = false;
    for (const auto& d : parsed.diagnostics) {
        errors = errors || d.severity() == diag::Severity::Error;
        m_diagnostics.push_back(to_info(d, m_text_info.get()));
    }
    const bool parse_errors = errors;
    m_ast = parsed.ast;
    if (!library || !parsed.ast)
        return;
    auto ir =
        compiler::compile_interpreted_ir(library->compiled(), std::move(parsed), library->config().interpreted_version);
    for (const auto& d : ir.diagnostics) {
        errors = errors || d.severity() == diag::Severity::Error;
        m_diagnostics.push_back(to_info(d, m_text_info.get()));
    }
    if (!ir.graph)
        return;
    m_graph = ir.graph;
    m_graph_text = m_text_info;
    m_graph_library = library;
    if (!parse_errors)
        m_parsed = { m_graph, m_graph_text, m_graph_library };
    // As sclang, the backend only runs on code without errors.
    if (errors)
        return;
    const auto compiled = compiler::compile_interpreted(library->compiled(), ir);
    for (const auto& d : compiled.diagnostics)
        m_diagnostics.push_back(to_info(d, m_text_info.get()));
}

std::vector<DiagnosticInfo> Document::diagnostics() const { return m_diagnostics; }

}
