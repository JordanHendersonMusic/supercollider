#pragma once

#include "sc_backend/details/frames.hpp"
#include "sc_backend/details/members.hpp"

#include "sc_sema/details/indexes.hpp"
#include "sc_sema/details/symbols.hpp"
#include "sc_sema/details/ir_nodes_details.hpp"

#include "sc_util/indexed_vector.hpp"
#include "sc_util/strong_index.hpp"

#include <cassert>
#include <cstdint>
#include <limits>
#include <variant>

/// @file
/// @brief The lowered ir: each function's instructions in the order the VM runs them, operands first, with locals,
/// members and nested functions resolved. How each is encoded is the emitter's choice. Each body, the function's own
/// and those inlined into it, is a contiguous range of its instructions, so the emitter can measure them to lay out
/// jumps.
namespace sc::backend::lir {

////////////////////////////////////////////////////////////////////////////////
// Indexes
////////////////////////////////////////////////////////////////////////////////

/// @brief A function within one lowered graph.
using FunctionIndex = util::StrongIndex<std::uint32_t, struct FunctionIndex__>;
using OptionalFunctionIndex =
    util::StrongOptionalIndex<std::uint32_t, struct FunctionIndex__, std::numeric_limits<std::uint32_t>::max()>;
/// @name Indexes within one function
/// @{
/// An instruction of a function.
using InstructionIndex = util::StrongIndex<std::uint32_t, struct InstructionIndex__>;
/// A body of a function, see Body.
using BodyIndex = util::StrongIndex<std::uint32_t, struct BodyIndex__>;
using OptionalBodyIndex =
    util::StrongOptionalIndex<std::uint32_t, struct BodyIndex__, std::numeric_limits<std::uint32_t>::max()>;
using SwitchCaseIndex = util::StrongIndex<std::uint32_t, struct SwitchCaseIndex__>;
/// @}

/// @brief A slot the VM can address, see FrameSlotIndex.
using SlotIndex = util::StrongIndex<std::uint8_t, struct SlotIndex__>;

/// @brief A variable the VM can address.
struct LocalRef {
    std::uint16_t depth; ///< The number of frames up from the current one.
    SlotIndex slot; ///< The slot in that frame.
};
static_assert(sizeof(LocalRef) == 4);

////////////////////////////////////////////////////////////////////////////////
// Pushes and stores
////////////////////////////////////////////////////////////////////////////////

/// @brief A value with its own push opcode.
enum struct SpecialValue : std::uint8_t { Nil, True, False };

/// @name Pushes
/// Instructions that push a value on the stack.
/// @{

/// @brief Pushes a variable.
struct PushLocal {
    LocalRef local;
};
/// @brief Pushes an instance variable.
struct PushInstance {
    InstanceSlotIndex slot;
};
/// @brief Pushes a class variable.
struct PushClassVariable {
    ClassVariableIndex index;
};
/// @brief Pushes a constant.
struct PushConstant {
    ir::ConstExprIndex value;
};
/// @brief Pushes a constant through the class's own object, as the old compiler did, kept for byte parity.
/// 3.16 uses it only for mutable constants.
struct PushClassConstant {
    ir::MemberIndex member;
    ir::ConstExprIndex value;
};
/// @brief Pushes `nil`, `true`, or `false`.
struct PushSpecialValue {
    SpecialValue value;
};
/// @brief Pushes an integer.
struct PushInteger {
    std::int32_t value;
};
/// @brief Pushes a symbol.
struct PushSymbol {
    ir::SymSCSymbol value;
};
/// @brief Pushes a class.
struct PushClass {
    ir::SymSCClassName name;
};
/// @brief Pushes `this`, `thisProcess`, or another special value.
struct PushSpecial {
    ir::SCSpecial::Kind kind;
};
/// @brief Pushes a closure of a nested function.
struct PushFunction {
    FunctionIndex function;
};

/// @}

/// @name Stores
/// Stores keep the value on the stack, the emitter folds a following Drop into them.
/// @{

/// @brief Stores to a variable.
struct StoreLocal {
    LocalRef local;
};
/// @brief Stores to an instance variable.
struct StoreInstance {
    InstanceSlotIndex slot;
};
/// @brief Stores to a class variable.
struct StoreClassVariable {
    ClassVariableIndex index;
};

/// @}

/// @brief Duplicates the top of the stack.
struct Dup {};
/// @brief Removes the top of the stack.
struct Drop {};

////////////////////////////////////////////////////////////////////////////////
// Sends and returns
////////////////////////////////////////////////////////////////////////////////

/// @brief Whether a send is the last thing a function does, so its result is returned straight away.
enum struct Tail : std::uint8_t { None, ReturnFromFunction, ReturnFromMethod };

/// @brief How the receiver is looked up: normally, from the superclass, or with an adverb.
enum struct SendKind : std::uint8_t { Normal, Super, WithAdverb };

/// @brief How the send was written, which the old compiler chose bytecodes by.
/// Implicit is a send lowering adds, e.g., `new`.
enum struct SendForm : std::uint8_t { Call, Binary, Setter, Implicit };

/// @brief Sends a message. The receiver and arguments are on the stack.
struct Send {
    ir::SymSCSelector selector; ///< The message name.
    std::uint8_t num_positional; ///< The number of positional arguments. An adverb is counted as the last one.
    std::uint8_t num_keyword; ///< The number of keyword arguments.
    SendKind kind : 2; ///< How the receiver is looked up.
    SendForm form : 2; ///< How the send was written.
    Tail tail : 2; ///< Whether it is the last thing the function does.
};

/// @brief Returns the top of the stack from the function.
struct Return {};
/// @brief Returns `this`. A method's end when it has no `^`.
struct ReturnSelf {};
/// @brief Returns from the method that contains the function.
/// `^` in a function, but only a non-inlined function. `Class { meth { if (a) { ^1 }}}` this is not non-local.
struct NonLocalReturn {};

/// @brief Computes the argument when it is nil, its prototype doesn't hold its default.
struct DefaultArgument {
    SlotIndex slot; ///< The argument.
    BodyIndex value; ///< The body that computes the default.
};

////////////////////////////////////////////////////////////////////////////////
// Inlined sends
////////////////////////////////////////////////////////////////////////////////

/// @name Inlined sends
/// See Inlined. The tested value is on the stack. Each body leaves its value, but an empty one leaves nothing, as the
/// old compiler had special forms for empty `if` bodies.
/// @{

/// @brief `if`, with the tested value on the stack.
struct If {
    BodyIndex then_body; ///< Runs if true.
    OptionalBodyIndex else_body; ///< Runs if false, if given.
};
/// @brief `if (x.isNil)`.
struct IfNil {
    BodyIndex then_body; ///< Runs if nil.
    OptionalBodyIndex else_body; ///< Runs if not nil, if given.
};
/// @brief `if (x.notNil)`.
struct IfNotNil {
    BodyIndex then_body; ///< Runs if not nil.
    OptionalBodyIndex else_body; ///< Runs if nil, if given.
};
/// @brief `a and: { b }`.
struct And {
    BodyIndex rhs; ///< Runs if the left side is true.
};
/// @brief `a or: { b }`.
struct Or {
    BodyIndex rhs; ///< Runs if the left side is false.
};
/// @brief `a ?? { b }`.
struct OrIfNil {
    BodyIndex rhs; ///< Runs if the left side is nil.
};
/// @brief `a !? { b }`.
struct AndIfNotNil {
    BodyIndex rhs; ///< Runs if the left side is not nil.
};
/// @brief `a ? b`. Both operands are on the stack.
struct QuestionMark {};
/// @brief `{ c }.while { b }`. Pushes nil when it ends.
struct While {
    BodyIndex condition; ///< Computes whether to continue.
    OptionalBodyIndex body; ///< Runs each time round, if given.
};
/// @brief `{ b }.loop`. Never ends, but counts as pushing a value.
struct Loop {
    BodyIndex body; ///< Runs each time round.
};

/// @brief One case of a Switch. The default has no key.
struct SwitchCase {
    ir::OptionalConstExprIndex key; ///< The value to match, none for the default.
    BodyIndex body; ///< Runs if matched.
};
static_assert(sizeof(SwitchCase) == 8);

/// @brief `switch`. Its cases are consecutive in Function::switch_cases, the default last.
struct Switch {
    SwitchCaseIndex first; ///< The first case.
    std::uint32_t count; ///< How many cases there are.
};
/// @}

/// @brief The indexes of a Switch's cases.
[[nodiscard]] inline util::IndexRange<SwitchCaseIndex> cases(const Switch& s) {
    return { s.first, util::advance(s.first, s.count) };
}

/// @brief Something that can't be lowered yet. Also used for malformed instructions.
struct Unimplemented {
    ir::NodeIndex node; ///< The node that could not be lowered.
};

/// @brief Any instruction.
using Instruction =
    std::variant<PushLocal, PushInstance, PushClassVariable, PushConstant, PushClassConstant, PushSpecialValue,
                 PushInteger, PushSymbol, PushClass, PushSpecial, PushFunction, StoreLocal, StoreInstance,
                 StoreClassVariable, Dup, Drop, Send, Return, ReturnSelf, NonLocalReturn, DefaultArgument, If, IfNil,
                 IfNotNil, And, Or, OrIfNil, AndIfNotNil, QuestionMark, While, Loop, Switch, Unimplemented>;
static_assert(sizeof(Instruction) == 12);

////////////////////////////////////////////////////////////////////////////////
// Functions
////////////////////////////////////////////////////////////////////////////////

/// @brief The bytes of the source an instruction came from.
struct SourceSpan {
    std::uint32_t begin; ///< The first byte.
    std::uint32_t end; ///< One past the last byte.
};
static_assert(sizeof(SourceSpan) == 8);

/// @brief A contiguous range of a function's instructions: the function's own, or one inlined into it.
struct Body {
    InstructionIndex begin; ///< The first instruction.
    std::uint32_t size; ///< How many instructions there are.
};
static_assert(sizeof(Body) == 8);

/// @brief The indexes of a Body's instructions.
[[nodiscard]] inline util::IndexRange<InstructionIndex> instructions(const Body& b) {
    return { b.begin, util::advance(b.begin, b.size) };
}

/// @brief One lowered function: a method, a function, or a generator.
struct Function {
    FrameIndex frame; ///< The frame it creates.
    ir::SourceIndex source; ///< The source its spans refer to.
    /// A root whose code, and its nested functions', all lowered, so can be emitted. Unset for nested functions.
    bool complete { false };
    util::IndexedVector<InstructionIndex, Instruction> instructions {}; ///< The instructions of every body.
    util::IndexedVector<InstructionIndex, SourceSpan> spans {}; ///< Where each instruction came from.
    util::IndexedVector<BodyIndex, Body> bodies {}; ///< The bodies, the function's own is function_body.
    util::IndexedVector<SwitchCaseIndex, SwitchCase> switch_cases {}; ///< The cases of every Switch.
};

/// @brief The body of the function itself.
inline constexpr BodyIndex function_body { 0 };

/// @brief The output of lower(). A root's nested functions follow it.
struct LoweredGraph {
    util::IndexedVector<FunctionIndex, Function> functions {}; ///< Every function.
};

}
