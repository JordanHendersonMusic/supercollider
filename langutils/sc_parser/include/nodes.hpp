// Copyright Jordan Henderson 2026
#pragma once

#include "index.hpp"
#include "indexes_typed.hpp"
#include "node_base.hpp"
#include <cstdint>


/*

This file should be read alongside the indexes_typed.hpp

The typed indexs define conversions. This allows integers to become expression, for example.

This file defines extra information ir nodes need, and what children they can have.


For example, say we have a basic language that has integers, float, expressions (nothing else), and functions which are
just a list of expressions.

We would then have the following typed indexes:

    using IntIndex = TypedIndex<IRType::IntLiteral>;
    using FloatIndex = TypedIndex<IRType::FloatLiteral>;
    using FunctionIndex = TypedIndex<IRType::FunctionLiteral>;

    // Expressions can either be ints, floats, or functions.
    using ExpressionIndex = join<IntIndex, FloatIndex, FunctionIndex>;

    using ExpressionListIndex = TypedIndex<IRType::ExpressionList>;

Then the nodes would look like this:

    struct IntLiteral : public priv::TerminalNode<IntLiteral, IntIndex> {};
    struct FloatLiteral : public priv::TerminalNode<FloatLiteral, FloatIndex> {};

    struct FunctionLiteral : public priv::Node<
        FunctionLiteral(ExpressionListIndex body),  <<<<<<< 1.
        FunctionIndex
    > {};

    struct ExpressionList : public priv::ListNode<
        ExpressionList,
        ExpressionListIndex,
        ExpressionIndex  <<<<<<<<<< 2.
    > {};


    //      1. this line defines that the constructor of a function literal node requires an expression list be passed.
This is a function signature type.

    //      2. this line defines that the expression list can have any number of expressions appended to it.

Note how there is no IR node the corresponds to an expression. This is because ints, floats, and functions can all be
valid expressions in their own right.

If IR nodes need extra information this can be done by simply create data in the struct.

Was the struct is defined, you MUST add it to the template at the end of this file.
This way, the structure will automatically be added to the variant in the ir graph.

The reason why all this complexity is desirable, is because when it comes to compile the ir graph, we know exactly what
all the children are. Although the grammar/parser may actually produce a more restricted graph. By compiling a more
general graph, we end up being able to change the grammar without worrying about how to compile it, so long as we don't
change the definitions below.

*/

namespace sc::parser::nodes {

struct Missing : public priv::TerminalNode<Missing, MissingIndex> {
    static constexpr auto name { "Missing" };
};

struct IntNode : public priv::TerminalNode<IntNode, IntLitIndex> {
    static constexpr auto name { "IntNode" };
    enum struct Kind : std::uint8_t { Normal, Radix, Hexadecimal } kind;
    enum struct Sign { Positive, Negative } sign;
    constexpr IntNode(Kind i = Kind::Normal, Sign s = Sign::Positive): kind(i), sign(s) {}
};

struct FloatNode : public priv::TerminalNode<FloatNode, FloatLitIndex> {
    static constexpr auto name { "FloatNode" };
    enum struct Kind : std::uint8_t { Normal, Radix, Exponent, Pi, Inf } kind;
    enum struct Sign { Positive, Negative } sign;
    constexpr FloatNode(Kind i = Kind::Normal, Sign s = Sign::Positive): kind(i), sign(s) {}
};

struct PiNode : public priv::Node<PiNode(maybe<NumberIndex> multiplier), PiLitIndex> {
    static constexpr auto name { "PiNode" };
    enum struct Sign { Positive, Negative } sign; // if the multiplier is signed, this is always positive.
    constexpr PiNode(Sign s = Sign::Positive): sign(s) {}
};

struct AccidentalNode : public priv::TerminalNode<AccidentalNode, AccidentalLitIndex> {
    static constexpr auto name { "AccidentalNode" };
    enum struct Kind { Steps, Cents } kind;
    enum struct Sign { Positive, Negative } sign;
    constexpr AccidentalNode(Kind k, Sign s = Sign::Positive): kind(k), sign(s) {}
};

struct StringLineNode : public priv::TerminalNode<StringLineNode, StringLineLitIndex> {
    static constexpr auto name { "StringLineNode" };
};

// This is the true string literal type
struct StringLineList : public priv::ListNode<StringLineList, StringLitIndex, StringLineLitIndex> {
    static constexpr auto name { "StringLineList" };
};

struct SymbolNode : public priv::TerminalNode<SymbolNode, SymbolLitIndex> {
    static constexpr auto name { "SymbolNode" };
    enum struct Kind { Quote, Slash } kind;
    constexpr SymbolNode(Kind k): kind(k) {}
};

struct BooleanNode : public priv::TerminalNode<BooleanNode, BooleanLitIndex> {
    static constexpr auto name { "BooleanNode" };
    bool value;
    constexpr BooleanNode(bool v): value(v) {}
};

struct NilNode : public priv::TerminalNode<NilNode, NilLitIndex> {
    static constexpr auto name { "NilNode" };
};

struct CurryNode : public priv::TerminalNode<CurryNode, CurryIndex> {
    static constexpr auto name { "CurryNode" };
};

struct NameNode : priv::TerminalNode<NameNode, NameIndex> {
    static constexpr auto name { "NameNode" };
};

struct SelectorNode : priv::TerminalNode<SelectorNode, SelectorIndex> {
    static constexpr auto name { "SelectorNode" };
    constexpr SelectorNode(bool infix = false) noexcept: is_infix_keyword(infix) {}
    bool is_infix_keyword;
};

struct PositionArgumentList : priv::ListNode<PositionArgumentList, PositionalArgumentPackIndex, ExprIndex> {
    static constexpr auto name { "PositionalArgumentList" };
};

struct KeywordArgumentList : priv::ListNode<KeywordArgumentList, KeywordArgumentPackIndex, KwArgIndex> {
    static constexpr auto name { "KeywordArgumentList" };
};

struct VariadicArgument : priv::Node<VariadicArgument(ExprIndex), VariadicArgumentPackIndex> {
    static constexpr auto name { "VariadicArgument" };
};

struct ArgumentList : priv::ListNode<ArgumentList, ArgumentListIndex, ArgumentEntryIndex> {
    static constexpr auto name { "ArgumentList" };
};

struct DeclInitialisationNode : priv::Node<DeclInitialisationNode(ExprIndex), DeclInitialisationIndex> {
    static constexpr auto name { "DeclInitialisationNode" };
    enum struct Kind : std::uint8_t { PreserveNil, OverrideNil } kind;
    constexpr DeclInitialisationNode(Kind i): kind(i) {}
};

struct DeclNode : priv::Node<DeclNode(NameIndex name, maybe<ExprIndex> default_value), DeclIndex> {
    static constexpr auto name { "DeclNode" };
    enum struct ReadWriteAccessor : std::uint8_t { Omitted, Read, Write, ReadAndWrite } accessor;
    enum struct Kind : std::uint8_t { Argument, BlockVariable, ClassInstanceVariable, ClassVariable, Const } kind;
    constexpr DeclNode(ReadWriteAccessor r, Kind k): accessor(r), kind(k) {}
};

struct KwArgNode : priv::Node<KwArgNode(SymbolLitIndex keyword, ExprIndex value), KwArgIndex> {
    static constexpr auto name { "KwArgNode" };
};


struct ExprList : priv::ListNode<ExprList, ExprListIndex, ExprIndex> {
    static constexpr auto name { "ExprList" };
};

// clang-format off
struct MessageNode : priv::Node<
    MessageNode(
        ExprIndex receiver,
        SelectorIndex selector, 
        maybe<ArgumentAnyIndex> args
    ),
    MessageIndex
> { 
    static constexpr auto name {"MessageNode"};

};
// clang-format on

struct FunctionNode : priv::Node<FunctionNode(ExprListIndex body), FunctionLitIndex> {
    static constexpr auto name { "FunctionNode" };
};


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

using NodeCollection =
    priv::IRNodeCollectionHelper<Missing, IntNode, FloatNode, PiNode, AccidentalNode, StringLineNode, StringLineList,
                                 SymbolNode, BooleanNode, NilNode, CurryNode, NameNode, SelectorNode,
                                 PositionArgumentList, KeywordArgumentList, VariadicArgument, ArgumentList,
                                 DeclInitialisationNode, DeclNode, KwArgNode, ExprList, MessageNode, FunctionNode>;

using NodeVariant = NodeCollection::variant;
// Useful for meta programming
using NodeTuple = NodeCollection::tuple;

};
