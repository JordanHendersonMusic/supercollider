#pragma once

#include "sc_backend/emit.hpp"
#include "sc_backend/details/frames.hpp"
#include "sc_backend/details/lir.hpp"

#include "sc_sema/details/source_location.hpp"
#include "sc_sema/details/symbols.hpp"
#include "sc_sema/details/indexes.hpp"

#include "sc_util/indexed_vector.hpp"

#include <cstdint>

namespace sc::compiler {

/// @brief An argument or variable of a compiled function.
struct CompiledSlot {
    ir::SymSCIdentifier name; ///< The name.
    /// The value in the prototype frame, which is nil unless the argument's default or the variable's initial value is
    /// a literal.
    ir::OptionalConstExprIndex prototype;
};
static_assert(sizeof(CompiledSlot) == 8);

/// @brief A compiled method or function.
struct CompiledFunction {
    ir::SourceLocation location; ///< Where it was written.
    ir::OptionalSymSCSymbol name; ///< Its name, if it has one.
    backend::lir::OptionalFunctionIndex parent; ///< The function it is nested in, none for a method.
    bool has_this; ///< Whether the first slot is `this`.
    std::uint32_t num_positional; ///< How many positional arguments it has.
    backend::Variadic variadic; ///< None, args, kwargs.
    /// As backend::Frame's.
    util::IndexedVector<backend::FrameSlotIndex, CompiledSlot> slots;
    backend::EmittedFunction emitted; ///< Its bytecode.
};
}
