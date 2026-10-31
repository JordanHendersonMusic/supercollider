#pragma once

#include "sc_backend/details/frames.hpp"
#include "sc_backend/details/inlining.hpp"
#include "sc_backend/details/lir.hpp"
#include "sc_backend/details/members.hpp"

#include <ostream>

namespace sc::ir {
struct ConstantTable;
struct SymbolTable;
}

namespace sc::backend {

////////////////////////////////////////////////////////////////////////////////
// API
////////////////////////////////////////////////////////////////////////////////

struct LoweringSymbols;

/// @brief Everything lower() needs, all referred to rather than owned, so they must outlive the call.
struct LoweringContext {
    const ir::IRGraph& graph; ///< The graph being lowered.
    /// From layout_frames, given the same root and inlining.
    const Frames& frames;
    const MemberTable& members; ///< Where the class's members are stored.
    const ir::ConstantTable& constants; ///< The constants the graph refers to.
    ir::SymbolTable& symbols; ///< Where new symbols are interned.
    const LoweringSymbols& selectors; ///< The selectors lowering uses for sends the source does not write.
    const InliningContext& inlining; ///< Decides which sends are inlined.
    ir::SymSCClassName array; ///< The `Array` class.
    ir::SymSCClassName event; ///< The `Event` class.
    /// A poisoned method, or one that can't be lowered, is lowered to throw this, with `thisMethod` and its errors,
    /// which the latter doesn't have. Without it, such a method is skipped.
    ir::OptionalSymSCClassName poisoned_method_error;
};

/// @brief Lowers a class or interpreted code to instructions in the order the VM runs them.
///
/// A class's poisoned methods are lowered as throwing ctx.poisoned_method_error, as are those that fail to lower, after
/// their failed functions.
/// @param ctx What lowering needs.
/// @param root The class or interpreted code. It cannot be a poisoned method nor one that is too large for the VM.
/// @return Every function, with the root's first and its nested functions following.
[[nodiscard]] lir::LoweredGraph lower(const LoweringContext& ctx, ir::join<ir::SCClass_I, ir::SCInterpreted_I> root);

/// @brief Writes an instruction as text, for debugging and tests.
void write_instruction(std::ostream& out, const lir::Function& function, const lir::Instruction& instruction,
                       const ir::IRGraph& graph, const ir::SymbolTable& symbols, const ir::ConstantTable& constants);

////////////////////////////////////////////////////////////////////////////////
// Lowering symbols
////////////////////////////////////////////////////////////////////////////////

/// @brief Selectors lowering uses for sends that the source doesn't write.
struct LoweringSymbols {
    /// @brief Interns all the selectors.
    explicit LoweringSymbols(ir::SymbolTable& symbols);

    ir::SymSCSelector new_; ///< `new`.
    ir::SymSCSelector throw_; ///< `throw`.
    ir::SymSCSelector add; ///< `add`.
    ir::SymSCSelector put; ///< `put`.
    ir::SymSCSelector at; ///< `at`.
    ir::SymSCSelector copy; ///< `copy`.
    ir::SymSCSelector copy_to_end; ///< `copyToEnd`.
    /// `prSimpleNumberSeries`, the old grammar's s_series, not the special selector `series`.
    ir::SymSCSelector series;
    ir::SymSCSelector envir_get; ///< Reads an environment variable.
    ir::SymSCSelector envir_put; ///< Assigns an environment variable.
    ir::SymSCSelector perform_list; ///< `performList`, for variadic arguments.
    ir::SymSCSelector super_perform_list; ///< `performList` sent to `super`.
    /// `case { c }` is sent as `c.if`.
    ir::SymSCSelector if_;
    /// @name Generators, as the old grammar rewrote them.
    /// @{
    ir::SymSCSelector r;
    ir::SymSCSelector do_;
    ir::SymSCSelector for_series;
    ir::SymSCSelector value;
    ir::SymSCSelector yield;
    ir::SymSCSelector always_yield;
    ir::SymSCSelector series_iter;
    /// @}
};
}
