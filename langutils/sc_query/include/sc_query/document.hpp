#pragma once

/// @file
/// @brief An editor's document, or a snippet, and what compiling it found.

#include "sc_query/library.hpp"
#include "sc_query/text.hpp"

#include <filesystem>
#include <memory>
#include <string>
#include <vector>

namespace sc::ast {
struct ASTGraph;
}
namespace sc::ir {
struct IRGraph;
}

namespace sc::query {

////////////////////////////////////////////////////////////////////////////////
// API
////////////////////////////////////////////////////////////////////////////////

/// @brief How a document is compiled.
enum struct DocumentKind : std::uint8_t {
    ClassFile, ///< A .sc file, compiled as part of the library.
    Interpreted, ///< Anything else, e.g., an .scd file, compiled as regions against the library.
};

/// @brief The kind of a document from its path, a .sc file is a class file.
[[nodiscard]] DocumentKind kind_of(const std::filesystem::path& path);

/// @brief How serious a diagnostic is. Information is for the tools' own hints, e.g., a selector nothing implements.
enum struct Severity : std::uint8_t { Error, Warning, Information };

/// @brief A diagnostic as the tools hand them out.
struct DiagnosticInfo {
    Severity severity { Severity::Error };
    std::string name; ///< e.g., `Unexpected Token.`
    std::string message; ///< The first location's message, and any extra explanation, as one sentence or more.
    Range range; ///< In the document.
    /// The other locations, each with its message. A location without a path is in the document.
    std::vector<std::pair<Location, std::string>> related;
    std::string full; ///< As sclang prints it, with the source highlighted.
};

/// @brief A compile of interpreted code: its graph, and the text and library it was compiled from.
struct CompiledText {
    std::shared_ptr<const ir::IRGraph> graph {};
    std::shared_ptr<const src::TextInfo> text {};
    std::shared_ptr<Library> library {};
};

/// @brief A document's text, and the last compile of it.
class Document {
public:
    Document(std::filesystem::path path, std::string text, DocumentKind kind);
    ~Document();
    // The line index refers to the text.
    Document(const Document&) = delete;
    Document& operator=(const Document&) = delete;

    /// @brief Replaces the text. The analysis stays until analyse() is called again.
    void set_text(std::string text);
    [[nodiscard]] const std::string& text() const { return m_text; }
    [[nodiscard]] const LineIndex& lines() const { return m_lines; }
    /// @brief The file, normal_path()'d, empty for an unsaved document.
    [[nodiscard]] const std::filesystem::path& path() const { return m_path; }
    [[nodiscard]] DocumentKind kind() const { return m_kind; }
    /// @brief Whether the text changed since the last analyse().
    [[nodiscard]] bool stale() const { return m_stale; }

    /// @brief Compiles the text: parses a class file, compiles interpreted code against the library.
    /// A class file's own compile is the library's, see Library::text_of().
    void analyse(const std::shared_ptr<Library>& library);

    /// @brief What compiling found in this document. A class file's come from the library too, when it was built
    /// with this text.
    [[nodiscard]] std::vector<DiagnosticInfo> diagnostics() const;

    /// @brief The library the last analyse() used.
    [[nodiscard]] const std::shared_ptr<Library>& library() const { return m_library; }
    /// @brief The last analysed text.
    [[nodiscard]] const std::shared_ptr<const src::TextInfo>& text_info() const { return m_text_info; }
    /// @brief The last analysed text's parse, null when it couldn't be parsed, or for a class file the library compiled
    /// with this text.
    [[nodiscard]] const std::shared_ptr<const ast::ASTGraph>& ast() const { return m_ast; }
    /// @brief The interpreted code's graph, null for a class file, or when it couldn't be compiled. Kept from the last
    /// compile that made one, so it can be older than the text.
    [[nodiscard]] const std::shared_ptr<const ir::IRGraph>& graph() const { return m_graph; }
    /// @brief The text the graph was compiled from.
    [[nodiscard]] const std::shared_ptr<const src::TextInfo>& graph_text() const { return m_graph_text; }
    /// @brief The library the graph was compiled against, whose symbols it uses.
    [[nodiscard]] const std::shared_ptr<Library>& graph_library() const { return m_graph_library; }
    /// @brief The last compile of interpreted code that parsed without errors. The graph of code that doesn't parse
    /// misses much of it, so this one's names are better while code is being typed. Empty for a class file.
    [[nodiscard]] const CompiledText& parsed() const { return m_parsed; }

private:
    std::filesystem::path m_path;
    std::string m_text;
    LineIndex m_lines;
    DocumentKind m_kind;
    bool m_stale { true };

    std::shared_ptr<Library> m_library {};
    std::shared_ptr<const src::TextInfo> m_text_info {};
    std::shared_ptr<const ast::ASTGraph> m_ast {};
    std::shared_ptr<const ir::IRGraph> m_graph {};
    std::shared_ptr<const src::TextInfo> m_graph_text {};
    std::shared_ptr<Library> m_graph_library {};
    CompiledText m_parsed {};
    std::vector<DiagnosticInfo> m_diagnostics {};
};

/// @brief Turns a diagnostic into a DiagnosticInfo, its range the first location in @p text, or the start.
[[nodiscard]] DiagnosticInfo to_info(const diag::Diagnostic& d, const src::TextInfo* text);

}
