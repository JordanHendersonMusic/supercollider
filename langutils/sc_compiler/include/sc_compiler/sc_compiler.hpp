#pragma once

/// @file
/// @brief What sclang needs. See inspect.hpp for the stages one by one, and for compiling from text rather than files.

#include "sc_compiler/compiled.hpp"
#include "sc_compiler/vm_definitions.hpp"

#include "sc_source/text_info.hpp"

#include "sc_util/indexed_vector.hpp"
#include "sc_util/language_version.hpp"
#include "sc_util/strong_index.hpp"

#include <chrono>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace sc::diag {
class Diagnostic;
}
namespace sc::ir {
// Same as in sc_sema/details/class_library_sources.hpp, so only the users need SourceEntry.
struct SourceEntry;
using SourceEntryIndex = util::StrongIndex<std::uint32_t, struct SourceEntryIndex__>;
using ClassLibrarySources = util::IndexedVector<SourceEntryIndex, std::optional<SourceEntry>>;
}

namespace sc::ast {
struct ASTGraph;
}

namespace sc::compiler {

struct ClassLibrary;
struct ClassLibraryInternals;
struct InterpretedParsed;
struct InterpretedIR;
struct ClassLibraryTimes;

/// @brief A file of the class library.
struct ClassFile {
    std::filesystem::path path; ///< Where the file is.
    ir::ModuleIndex module; ///< The module it is in.
};
/// @brief Every file of the class library.
using ClassFiles = util::IndexedVector<ir::SourceEntryIndex, ClassFile>;


////////////////////////////////////////////////////////////////////////////////
// API
////////////////////////////////////////////////////////////////////////////////

/// @brief Parses and compiles the class library from files, up to the IR of every class.
///
/// The result is the information that needs to be kept and passed to every compile invocation. The loader needs access
/// to it.
/// @param files Every file of the class library.
/// @param num_threads How many threads to use.
/// @param vm What the VM defines itself.
/// @param times If given, set to how long each stage took.
/// @return The library, which holds the diagnostics found.
[[nodiscard]] std::unique_ptr<ClassLibrary> compile_class_library(const ClassFiles& files, std::size_t num_threads,
                                                                  const VMDefinitions& vm,
                                                                  ClassLibraryTimes* times = nullptr);

/// @brief A class graph's topological position: a superclass has a lower index than its subclasses.
using ClassGraphIndex = util::StrongIndex<std::uint32_t, struct ClassGraphIndex__>;

/// @brief Runs the backend's stages for every class of the library.
/// @param library The library from compile_class_library().
/// @param num_threads How many threads to use.
/// @return The compiled classes, a class graph for each class that is not poisoned.
[[nodiscard]] util::IndexedVector<ClassGraphIndex, CompiledGraph> compile_class_graphs(ClassLibrary& library,
                                                                                       std::size_t num_threads);

/// @brief Parses interpreted code, e.g., an editor's text or an scd file, after the class library.
[[nodiscard]] InterpretedParsed parse_interpreted(std::shared_ptr<const src::TextInfo> text);
/// @brief Compiles parsed interpreted code to IR, as if written inside `Interpreter:functionCompileContext`.
/// @param library The library from compile_class_library().
/// @param parsed The result of parse_interpreted().
/// @param version The language version of the code.
[[nodiscard]] InterpretedIR compile_interpreted_ir(ClassLibrary& library, InterpretedParsed&& parsed,
                                                   ast::LanguageVersion version);

/// @brief Runs the backend's stages for interpreted code, as they are for a class.
/// @param library The library from compile_class_library().
/// @param ir The result of compile_interpreted_ir(), whose graph must not be null.
[[nodiscard]] CompiledGraph compile_interpreted(ClassLibrary& library, const InterpretedIR& ir);

////////////////////////////////////////////////////////////////////////////////
// Class files
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// Class library
////////////////////////////////////////////////////////////////////////////////

/// @brief How long each stage took.
struct ClassLibraryTimes {
    std::chrono::duration<double> parse {}; ///< Parsing, which includes reading the files.
    std::chrono::duration<double> sema {}; ///< Compiling to IR.
    std::chrono::duration<double> member_table {}; ///< Laying out the members.
};

/// @brief The compiled class library. Global per vm instance.
/// Its parts are behind `internals`, see details/class_library_internals.hpp, for the tools.
struct ClassLibrary {
    /// @param class_sources The parsed files of the class library.
    explicit ClassLibrary(ir::ClassLibrarySources class_sources);
    ClassLibrary(const ClassLibrary&) = delete;
    ClassLibrary& operator=(const ClassLibrary&) = delete;
    ~ClassLibrary();

    /// Per file, in order: files that couldn't be opened or parsed.
    std::string parse_log {};
    std::vector<diag::Diagnostic> parse_diagnostics {}; ///< What was found parsing the files.
    std::vector<diag::Diagnostic> diagnostics {}; ///< What was found compiling the classes.

    /// @brief The text of a symbol.
    [[nodiscard]] std::string_view symbol_text(ir::Symbol symbol) const;
    /// @brief Whether a class name is a meta class.
    [[nodiscard]] bool is_meta(ir::SymSCClassName name) const;
    /// @brief The constants of the library.
    [[nodiscard]] const ir::ConstantTable& constants() const;
    /// @brief The number of class variables of every class together.
    [[nodiscard]] std::uint32_t num_class_variables() const;

    std::unique_ptr<ClassLibraryInternals> internals; ///< What the stages share.
};

////////////////////////////////////////////////////////////////////////////////
// Interpreted code
////////////////////////////////////////////////////////////////////////////////

/// @brief Parsed interpreted code.
struct InterpretedParsed {
    std::shared_ptr<const ast::ASTGraph> ast; ///< The parsed code. Null if it can't be parsed, or is a class file.
    std::vector<diag::Diagnostic> diagnostics; ///< What was found parsing it.
};

/// @brief Interpreted code as IR.
struct InterpretedIR {
    /// The IR. Null if it wasn't parsed, or the library has no Interpreter to compile it in. Its root is an
    /// ir::SCInterpreted_I.
    std::shared_ptr<const ir::IRGraph> graph;
    std::vector<diag::Diagnostic> diagnostics; ///< What was found compiling it.
};

}