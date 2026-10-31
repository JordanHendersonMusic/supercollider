#pragma once

#include "sc_backend/details/inlining.hpp"
#include "sc_backend/details/members.hpp"
#include "sc_backend/lower.hpp"
#include "sc_sema/compiler.hpp"
#include "sc_sema/ir.hpp"
#include "sc_sema/symbol_table.hpp"

namespace sc::compiler {

/// @brief What every graph's backend stages share, built once the class library is compiled.
struct Backend {
    /// @brief Builds the member table, and interns the selectors the backend uses.
    Backend(const ir::IRCompiler& compiler, ir::SymbolTable& symbols, const ir::ConstantTable& constants);
    Backend(const Backend&) = delete;
    Backend& operator=(const Backend&) = delete;

    backend::MemberTable members; ///< Where every member is stored.
    backend::LoweringSymbols selectors; ///< The selectors lowering uses.
    backend::SpecialSymbols special_symbols; ///< The selectors and classes that have their own bytecodes.
    backend::InliningContext inlining; ///< Contains a const & to the special_symbols.
    /// The name of the class a poisoned method throws.
    static constexpr const char* poisoned_method_error_name { "PoisonedMethodError" };
    /// The class a poisoned method throws, see backend::LoweringContext. Unset when the library doesn't define it.
    ir::OptionalSymSCClassName poisoned_method_error {};
};
}
