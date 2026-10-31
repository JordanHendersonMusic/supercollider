#pragma once

#include "sc_sema/compiler.hpp"
#include "sc_sema/context.hpp"
#include "sc_sema/ir.hpp"
#include "sc_sema/module.hpp"
#include "sc_sema/symbol_table.hpp"

#include <memory>

namespace sc::compiler {

struct Backend;


/// @brief What the stages share, behind ClassLibrary::internals. The members refer to each other, so it doesn't move.
/// Most users don't need access to this, but it is used for introspection, which the tests need.
struct ClassLibraryInternals {
    /// @param class_sources The parsed files of the class library.
    explicit ClassLibraryInternals(ir::ClassLibrarySources class_sources);
    ClassLibraryInternals(const ClassLibraryInternals&) = delete;
    ClassLibraryInternals& operator=(const ClassLibraryInternals&) = delete;
    ~ClassLibraryInternals();

    ir::SymbolTable symbols {}; ///< Every symbol.
    ir::ModuleTable modules {}; ///< The paths given to the compiler.
    /// By the index of the class file each was parsed from.
    ir::ClassLibrarySources sources;
    ir::SymSCClassName abstract_object; ///< The class every class inherits from.
    ir::ContextPtr ctx; ///< The compilation context.
    ir::IRCompiler compiler; ///< Compiles the classes to IR.
    /// Set by compile_class_library, see details/backend.hpp.
    std::unique_ptr<Backend> backend;
};
}
