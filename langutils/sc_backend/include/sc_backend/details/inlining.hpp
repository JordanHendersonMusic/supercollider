#pragma once

#include "sc_bytecode/details/OpcodeOperands.h"

#include "sc_sema/details/indexes.hpp"
#include "sc_sema/details/symbols.hpp"

#include "sc_util/indexed_vector.hpp"
#include "sc_util/language_version.hpp"
#include "sc_util/strong_index.hpp"

#include <cstdint>
#include <optional>
#include <unordered_map>
#include <variant>
#include <vector>

namespace sc::ir {
struct ConstantTable;
struct SymbolTable;
struct IRGraph;
}
namespace sc::ast {
struct ASTGraph;
}

namespace sc::backend {

////////////////////////////////////////////////////////////////////////////////
// API
////////////////////////////////////////////////////////////////////////////////

enum struct Inlined : std::uint8_t;
struct SpecialSymbols;

/// @brief What deciding whether a send is inlined needs, referred to rather than owned.
struct InliningContext {
    const SpecialSymbols& symbols; ///< The selectors that can be inlined.
    const ir::ConstantTable& constants; ///< The constants the graph refers to.
};

/// @brief Which kind of inlining, if any, a send gets.
/// The old compiler's rule: only functions without arguments or variables are inlined.
/// From 3.16 a function with variables that aren't captured can be inlined too, they are hoisted.
/// @param ctx What deciding needs.
/// @param g The graph.
/// @param m The send.
/// @param version The language version, as the rules depend on it.
/// @return The kind of inlining, empty if the send is not inlined.
[[nodiscard]] std::optional<Inlined> inlining(const InliningContext& ctx, const ir::IRGraph& g, ir::SCMessage_I m,
                                              ast::LanguageVersion version);

/// @brief Whether the receiver of an inlined send is itself inlined: `{ c }.while` inlines its receiver, `if (c)`
/// evaluates it.
[[nodiscard]] bool inlines_receiver(Inlined kind);

////////////////////////////////////////////////////////////////////////////////
// Inlined sends
////////////////////////////////////////////////////////////////////////////////

/// @brief The sends the compiler turns into jumps rather than sending.
enum struct Inlined : std::uint8_t {
    If, ///< `if (c) { a } { b }`
    IfNil, ///< `if (x.isNil) { a } { b }`, isNil isn't sent.
    IfNotNil, ///< `if (x.notNil) { a } { b }`
    And, ///< `a and: { b }`
    Or, ///< `a or: { b }`
    OrIfNil, ///< `a ?? { b }`
    AndIfNotNil, ///< `a !? { b }`
    QuestionMark, ///< `a ? b`, inlines nothing, it has its own bytecode.
    While, ///< `{ c }.while { b }`, `{ c }.while`
    WhileTrue, ///< `true.while { b }`
    Loop, ///< `{ b }.loop`
    Case, ///< `case { c } { a } { e }`, as nested ifs.
    Switch, ///< `switch (x, 1, { a }, { e })`
};

/// @brief Whether an expression is a function that can be inlined into the function around it.
[[nodiscard]] bool is_inlineable(const ir::IRGraph& g, ir::SCAnyExpr_I i, ast::LanguageVersion version);

/// @brief The position of an argument of a send.
using ArgumentIndex = util::StrongIndex<std::uint32_t, struct ArgumentIndex__>;
/// @brief The positional arguments of a send, in order.
using Arguments = util::IndexedVector<ArgumentIndex, ir::SCAnyExpr_I>;

/// @brief The positional arguments of a send, in order.
[[nodiscard]] Arguments positional_arguments(const ir::IRGraph& g, ir::SCMessage_I m);

////////////////////////////////////////////////////////////////////////////////
// Nil tests
////////////////////////////////////////////////////////////////////////////////

/// @brief `x.isNil` or `x.notNil`.
struct NilTest {
    bool is_nil; ///< Whether it is `isNil`, as opposed to `notNil`.
    ir::SCMessage_I message; ///< The send.

    /// @brief The inlined send this test makes.
    [[nodiscard]] Inlined kind() const { return is_nil ? Inlined::IfNil : Inlined::IfNotNil; }
};

/// @brief Whether an expression is a nil test.
[[nodiscard]] std::optional<NilTest> nil_test(const InliningContext& ctx, const ir::IRGraph& g, ir::SCAnyExpr_I i);

/// @brief Whether a function's body is a nil test, as a `case` condition `{ x.isNil }` is tested as an `if` would.
[[nodiscard]] std::optional<NilTest> nil_test_body(const InliningContext& ctx, const ir::IRGraph& g,
                                                   ir::SCFunction_I f);

////////////////////////////////////////////////////////////////////////////////
// Switch keys
////////////////////////////////////////////////////////////////////////////////

/// @brief The key of a switch case, if the expression can be one.
/// An integer, float, char, symbol or boolean literal, or a function returning one, though `{ 1 } == 1` is false when
/// sent. Not nil, which marks an empty slot of the table.
[[nodiscard]] std::optional<ir::ConstExprIndex> switch_key(const InliningContext& ctx, const ir::IRGraph& g,
                                                           ir::SCAnyExpr_I i, ast::LanguageVersion version);

////////////////////////////////////////////////////////////////////////////////
// Special symbols
////////////////////////////////////////////////////////////////////////////////

/// @brief Methods whose code is replaced by extended opcodes that loop using the method's frame, e.g., `Integer:do`.
enum struct HardCodedLoop : std::uint8_t {
    IntegerDo,
    IntegerReverseDo,
    IntegerFor,
    IntegerForBy,
    ArrayedCollectionDo,
    ArrayedCollectionReverseDo,
    DictionaryKeysValuesArrayDo,
    NumberForSeries,
    FloatDo,
    FloatReverseDo,
};

/// @brief A method that is a hard coded loop.
struct HardCodedLoopMethod {
    ir::SymSCClassName owner; ///< The class that defines the method.
    ir::SymSCSelector selector; ///< The method's name.
    HardCodedLoop loop; ///< Which loop it is.
};
static_assert(sizeof(HardCodedLoopMethod) == 12);

/// @brief The selectors and classes that have their own bytecodes, and the methods that are hard coded loops.
struct SpecialSymbols {
    /// @brief Interns all the symbols.
    explicit SpecialSymbols(ir::SymbolTable& symbols);

    /// A selector with its own opcode.
    using SpecialSelector = std::variant<OpSpecialSelectors, OpUnaryMath, OpBinaryMath>;

    /// The selectors with their own opcodes.
    std::unordered_map<ir::Symbol, SpecialSelector, std::hash<ir::Symbol>> special_selectors;
    /// The classes with their own opcodes.
    std::unordered_map<ir::Symbol, OpSpecialClassEnum, std::hash<ir::Symbol>> special_classes;
    ir::SymSCSelector series; ///< `series`.
    /// `doesNotUnderstand`, a method that never gets a shortcut.
    ir::SymSCSelector does_not_understand;
    /// `functionCompileContext`, a method that never gets a shortcut.
    ir::SymSCSelector function_compile_context;
    std::vector<HardCodedLoopMethod> hard_coded_loops; ///< The methods that are hard coded loops.

    /// @brief The special selector for a symbol, empty if it has none.
    [[nodiscard]] std::optional<SpecialSelector> find(ir::Symbol selector) const;
    /// @brief Which loop a method is, empty if it is not one. Of an instance method, also in an extension.
    [[nodiscard]] std::optional<HardCodedLoop> find_hard_coded_loop(ir::SymSCClassName owner,
                                                                    ir::SymSCSelector selector) const;
};

}
