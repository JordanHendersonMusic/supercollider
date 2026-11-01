#pragma once

/// @file
/// @brief The queries, free of any protocol: what is at a position, completion, symbols, and lookups by name.

#include "sc_query/document.hpp"
#include "sc_query/hooks.hpp"
#include "sc_query/library.hpp"
#include "sc_query/text.hpp"

#include "sc_sema/details/indexes.hpp"

#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace sc::ir {
struct IRGraph;
}

namespace sc::query {

////////////////////////////////////////////////////////////////////////////////
// API
////////////////////////////////////////////////////////////////////////////////

/// @brief A name in compiled code, and what it refers to.
struct Occurrence {
    enum struct Kind : std::uint8_t { Local, Member, Environment, Class, Send, Method, Keyword, Symbol };
    Kind kind { Kind::Local };
    Range range {}; ///< Where it is written.
    std::string name; ///< The variable, class, selector, keyword or symbol.
    bool declaration { false }; ///< Whether this declares it.
    /// For Local: the graph and local. For Send and Keyword: the graph of the message.
    const ir::IRGraph* graph { nullptr };
    std::optional<ir::LocalIndex> local {};
    bool argument { false }; ///< A Local that is an argument.
    std::string owner; ///< For Member: the declaring class.
    MemberKind member_kind { MemberKind::Instance };
    Location declared {}; ///< For Local and Member, where it is declared.
    std::optional<ClassId> receiver {}; ///< For Send and Keyword, the receiver's class when known.
    bool class_side { false }; ///< For Send and Keyword, the receiver is the class itself.
    std::string selector; ///< For Keyword, the message it is an argument of.
    std::optional<MethodId> method {}; ///< For Method, the method declared.
};

/// @brief A local variable in scope somewhere, for completion.
struct ScopeLocal {
    std::string name;
    Position declared {};
    Range scope {}; ///< The function or method declaring it.
    bool argument { false };
};

/// @brief The names in a document's last compile.
struct Analysis {
    std::vector<Occurrence> occurrences;
    std::vector<ScopeLocal> locals;
    /// The library the occurrences refer to, kept alive with them.
    std::shared_ptr<Library> library;
};

/// @brief The names in the document, from its interpreted graph, or the library's graphs of its class file.
[[nodiscard]] Analysis analyse_names(const Document& document);
/// @brief The smallest occurrence touching the position.
[[nodiscard]] const Occurrence* occurrence_at(const Analysis& analysis, Position p);

/// @brief Hover text, as markdown.
struct Hover {
    std::string markdown;
    Range range;
};
[[nodiscard]] std::optional<Hover> hover(const Document& document, Position p);

/// @brief Where what is at the position is declared. A location without a path is in the document.
[[nodiscard]] std::vector<Location> definition(const Document& document, Position p);

/// @brief Every use of what is at the position: locals in the document, everything else in the library and in the
/// given documents. A location without a path is in @p document.
[[nodiscard]] std::vector<Location> references(const Document& document, Position p, bool include_declaration,
                                               const std::vector<const Document*>& others = {});

/// @brief What a completion is.
enum struct CompletionKind : std::uint8_t { Class, Method, Variable, Member, Keyword, Argument, Symbol, Environment };

/// @brief One completion.
struct CompletionItem {
    std::string label;
    CompletionKind kind { CompletionKind::Variable };
    std::string detail;
    std::string documentation;
    std::string insert; ///< Empty to insert the label.
    std::uint8_t rank { 5 }; ///< Lower sorts first.
    std::optional<Range> replace; ///< Replaces more than Completions::replace, e.g. the `.` before a binary operator.
    std::string filter; ///< Empty to filter by the label.
};

/// @brief Completions at a position.
struct Completions {
    std::vector<CompletionItem> items;
    Range replace; ///< What is replaced: the word typed so far.
    bool incomplete { false }; ///< Cut short, so ask again as more is typed.
};
/// @brief Completes at a position, from the text before it, so it works while the code doesn't parse.
/// @param hooks Asked for symbol completions when connected, may be null.
[[nodiscard]] Completions complete(const Document& document, Position p, HookHost* hooks);

/// @brief A method signature, with where each parameter is in the label.
struct Signature {
    std::string label;
    std::vector<std::pair<std::uint32_t, std::uint32_t>> parameters; ///< Codepoint offsets into the label.
    std::string documentation;
};
/// @brief The signatures of the call the position is in.
struct SignatureHelp {
    std::vector<Signature> signatures;
    std::uint32_t active_signature { 0 };
    std::uint32_t active_parameter { 0 };
};
[[nodiscard]] std::optional<SignatureHelp> signature_help(const Document& document, Position p);

/// @brief A class, method, or variable in an outline.
struct SymbolInfo {
    enum struct Kind : std::uint8_t { Class, Method, ClassMethod, Variable, ClassVariable, Constant };
    std::string name;
    std::string detail;
    Kind kind { Kind::Class };
    Location location; ///< The whole of it.
    Range selection; ///< Its name.
    std::vector<SymbolInfo> children;
};
/// @brief The document's classes and methods, or an interpreted document's environment variables.
[[nodiscard]] std::vector<SymbolInfo> document_symbols(const Document& document);
/// @brief Classes and methods whose names contain the query, ignoring case.
[[nodiscard]] std::vector<SymbolInfo> workspace_symbols(const Library& library, std::string_view query,
                                                        std::size_t limit = 200);

/// @brief The tools' own hints: sends of a selector that no class implements, which fail unless the receiver handles
/// them in doesNotUnderstand, as an Event does. Only for a compile of the document's current text.
[[nodiscard]] std::vector<DiagnosticInfo> lint(const Document& document);
/// @brief The document's diagnostics, then lint()'s.
[[nodiscard]] std::vector<DiagnosticInfo> all_diagnostics(const Document& document);

/// @name Name queries
/// Readable text with file:line, for sc_mcp.
/// @{
/// @brief A class: its superclasses, subclasses, variables and methods.
[[nodiscard]] std::string describe_class(const Library& library, std::string_view name, bool inherited = false);
/// @brief Every class implementing a selector.
[[nodiscard]] std::string describe_implementors(const Library& library, std::string_view selector);
/// @brief Where something is declared: `Foo`, `Foo:bar`, `Foo.bar` or `Foo.*bar`, or a selector.
[[nodiscard]] std::string describe_definition(const Library& library, std::string_view name);
/// @brief The class tree under a class.
[[nodiscard]] std::string describe_subclasses(const Library& library, std::string_view name, std::size_t depth = 64);
/// @brief Classes and methods whose names contain the query.
[[nodiscard]] std::string describe_search(const Library& library, std::string_view query, std::size_t limit = 100);
/// @brief Every send of a selector, or use of a class, in the library.
[[nodiscard]] std::string describe_references(const Library& library, std::string_view name, std::size_t limit = 200);
/// @brief Diagnostics as text, each with file:line, or `No problems found.`
[[nodiscard]] std::string describe_diagnostics(const std::vector<DiagnosticInfo>& diagnostics,
                                               const std::string& file_name);
/// @}

}
