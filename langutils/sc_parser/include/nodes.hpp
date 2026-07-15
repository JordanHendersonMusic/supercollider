// Copyright Jordan Henderson 2026
#pragma once

#include "index.hpp"
#include "indexes_typed.hpp"
#include "node_base.hpp"
#include "sc_grammar_shared.hpp"
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

// This one is confusing.
// The Index type can be cast to from many of expression.
// But this node is solely used for a collection of expressions.
struct ExprSeq : priv::ListNode<ExprSeq, ExprSeqIndex, ExprSeqIndex> {
    static constexpr auto name { "ExprSeq" };
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
    enum struct Kind { Quote, Slash, KeyBinOp } kind;
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

struct ASCIINode : public priv::TerminalNode<ASCIINode, ASCIIIndex> {
    static constexpr auto name { "ASCIINode" };
};

struct CurryNode : public priv::TerminalNode<CurryNode, CurryIndex> {
    static constexpr auto name { "CurryNode" };
};


struct ArrayNode : public priv::ListNode<ArrayNode, ArrayIndex, ExprSeqIndex> {
    static constexpr auto name { "ArrayList" };
    // #[1, 2];
    // Previously, this would mean the compiler would create the array and you would just access it straight from the
    // constants slot. This is now not useful, because the new compiler can just look through all the elements of the
    // array and try to deduce them. Then, the new compiler, emits this into the constants slots, and COPIES THE OBJECT
    // to the stack when accessed. Hence the hash '#', means this is immutable, and you can therefore avoid a copy.
    bool is_immutable { false };
};

struct CollectionNode : public priv::Node<CollectionNode(ClassNameIdentifierIndex, ArrayIndex), CollectionIndex> {
    static constexpr auto name { "CollectionNode" };
};

struct DictionaryEntryNode
    : public priv::Node<DictionaryEntryNode(ExprSeqIndex key, ExprSeqIndex value), DictionaryEntryIndex> {
    static constexpr auto name { "DictionaryEntryNode" };
};

struct DictionaryNode : public priv::ListNode<DictionaryNode, DictionaryIndex, DictionaryEntryIndex> {
    static constexpr auto name { "DictionaryNode" };
};

struct BlockList : public priv::ListNode<BlockList, BlockListIndex, BlockIndex> {
    static constexpr auto name { "BlockList" };
};

struct NamedIdentifier : priv::TerminalNode<NamedIdentifier, NamedIdentifierIndex> {
    static constexpr auto name { "NameNode" };
};

struct ClassNameIdentifier : priv::TerminalNode<ClassNameIdentifier, ClassNameIdentifierIndex> {
    static constexpr auto name { "ClassNameIdentitier" };
};

struct PrimitiveIdentifier : priv::TerminalNode<PrimitiveIdentifier, PrimitiveIdentifierIndex> {
    static constexpr auto name { "PrimitiveIdentifier" };
};

struct EnvIdentifierNode : priv::Node<EnvIdentifierNode(NamedIdentifierIndex), EnvIdentifierIndex> {
    static constexpr auto name { "EnvIdentifierNode" };
};

struct SelectorNode : priv::TerminalNode<SelectorNode, TypedIndex<NodeFlag::SelectorLiteral>> {
    static constexpr auto name { "SelectorNode" };
    constexpr SelectorNode(bool infix = false) noexcept: is_infix_keyword(infix) {}
    bool is_infix_keyword;
};

struct SelectorWAdverb : priv::Node<SelectorWAdverb(SelectorIndex, AdverbIndex), SelectorWAdverbIndex> {
    static constexpr auto name { "SelectorNodeWithAdverb" };
};


struct VariadicArgNode : priv::Node<VariadicArgNode(ExprSeqIndex expr), VariadicArgIndex> {
    static constexpr auto name { "VariadicArgNode" };
};

struct ArgumentList : priv::ListNode<ArgumentList, ArgumentListIndex, ArgumentEntryIndex> {
    static constexpr auto name { "ArgumentList" };
};

struct KwArgNode : priv::Node<KwArgNode(SymbolLitIndex keyword, ExprSeqIndex value), KwArgIndex> {
    static constexpr auto name { "KwArgNode" };
};

struct AdverbExprNode : priv::Node<AdverbExprNode(ExprSeqIndex), AdverbExprIndex> {
    static constexpr auto name { "AdverbExprNode" };
};

struct MessageNode
    : priv::Node<MessageNode(maybe<SelectorMaybeAdverbIndex> selector, ArgumentListIndex args), MessageIndex> {
    static constexpr auto name { "MessageNode" };
    enum struct SelectorMode { SeeNode, Value, New, At } selector_mode;
    constexpr MessageNode(SelectorMode n = SelectorMode::SeeNode): selector_mode(n) {}
};

struct ReferenceNode : priv::Node<ReferenceNode(ExprSeqIndex), ReferenceIndex> {
    static constexpr auto name { "ReferenceNode" };
};

struct FunctionNode : priv::Node<FunctionNode(ExprSeqIndex body), FunctionLitIndex> {
    static constexpr auto name { "FunctionNode" };
};

struct AssignmentNode : priv::Node<AssignmentNode(NamedIdentifierIndex name, ExprSeqIndex expr), AssignmentIndex> {
    static constexpr auto name { "AssignmentNode" };
    enum struct Target { Normal, Environment } target;
    constexpr AssignmentNode(Target t = Target::Normal): target(t) {}
};

// This isn't a message node due to a nasty quirk in the grammar which was there in the original version.
struct AssignmentAtNode
    : priv::Node<AssignmentAtNode(ExprSeqIndex thing, ArgumentListIndex args, ExprSeqIndex value), AssignmentAtIndex> {
    static constexpr auto name { "AssignmentAt" };
};

struct SetterNode
    : priv::Node<SetterNode(join<ArgumentListIndex, ExprSeqIndex> thing, NamedIdentifierIndex name, ExprSeqIndex value),
                 SetterIndex> {
    static constexpr auto name { "SetterNode" };
};

struct DeclareArgumentWithDefaultNode
    : priv::Node<DeclareArgumentWithDefaultNode(NamedIdentifierIndex, ExprSeqIndex), DeclareArgumentWithDefaultIndex> {
    static constexpr auto name { "DeclareArgumentWithDefaultNode" };
    bool preserve_nil { false };
    constexpr DeclareArgumentWithDefaultNode(bool preserve_nil = false): preserve_nil(preserve_nil) {}
};

struct DeclareArgumentVariadicNode
    : priv::Node<DeclareArgumentVariadicNode(NamedIdentifierIndex), DeclareArgumentVariadicIndex> {
    static constexpr auto name { "DeclareArgumentVariadicNode" };
};

struct DeclareArgumentList : priv::ListNode<DeclareArgumentList, DeclareArgumentListIndex, DeclareAnyArgumentIndex> {
    static constexpr auto name { "DeclareArgumentList" };
};

struct DeclareVariableWithDefaultNode
    : priv::Node<DeclareVariableWithDefaultNode(NamedIdentifierIndex, ExprSeqIndex), DeclareVariableWithDefaultIndex> {
    static constexpr auto name { "DeclareVariableWithDefaultNode" };
};

struct DeclareVariableList : priv::ListNode<DeclareVariableList, DeclareVariableListIndex, DeclareAnyVariableIndex> {
    static constexpr auto name { "DeclareVariableList" };
};

struct BlockContentsList : priv::ListNode<BlockContentsList, BlockContentsListIndex, BlockItemIndex> {
    static constexpr auto name { "BlockContentsList" };
};

struct BlockNode : priv::Node<BlockNode(DeclareArgumentListIndex, maybe<BlockContentsListIndex>), BlockIndex> {
    static constexpr auto name { "BlockNode" };
};

struct NonLocalReturnExpr : priv::Node<NonLocalReturnExpr(ExprSeqIndex), NonLocalReturnExprIndex> {
    static constexpr auto name { "NonLocalReturnExpr" };
};

struct Method : priv::Node<Method(MethodNameIndex, DeclareArgumentListIndex, maybe<PrimitiveIdentifierIndex>,
                                  BlockContentsListIndex),
                           MethodIndex> {
    static constexpr auto name { "Method" };
};

struct ClassMethod : priv::Node<Method(MethodNameIndex, DeclareArgumentListIndex, maybe<PrimitiveIdentifierIndex>,
                                       BlockContentsListIndex),
                                ClassMethodIndex> {
    static constexpr auto name { "ClassMethod" };
};


struct MethodList : priv::ListNode<MethodList, MethodListIndex, AnyMethodIndex> {
    static constexpr auto name { "MethodList" };
};

struct DeclareClassVar : priv::Node<DeclareClassVar(DeclareAnyVariableIndex), DeclareClassVarIndex> {
    static constexpr auto name { "DeclareClassVar" };
    constexpr DeclareClassVar(ReadWriteAccessor rw = ReadWriteAccessor::Private): accessor(rw) {}
    ReadWriteAccessor accessor;
};

struct DeclareMemberList : priv::ListNode<DeclareMemberList, DeclareMemberListIndex, DeclareClassVarIndex> {
    static constexpr auto name { "DeclareClassVarList" };
};

struct DeclareClassMemberList
    : priv::ListNode<DeclareClassMemberList, DeclareClassMemberListIndex, DeclareClassVarIndex> {
    static constexpr auto name { "DeclareClassVarList" };
};

struct DeclareConstList : priv::ListNode<DeclareConstList, DeclareConstListIndex, DeclareClassVarIndex> {
    static constexpr auto name { "DeclareClassVarList" };
};

struct ClassAnyVarList : priv::ListNode<ClassAnyVarList, DeclareClassAnyVarListIndex, DeclareAnyList> {
    static constexpr auto name { "ClassVarList" };
};
struct Class
    : priv::Node<Class(NamedIdentifierIndex name, maybe<NamedIdentifierIndex> slot,
                       maybe<ClassNameIdentifierIndex> super, DeclareClassAnyVarListIndex vars, MethodListIndex meths),
                 ClassIndex> {
    static constexpr auto name { "Class" };
};

struct ClassExtension
    : priv::Node<ClassExtension(NamedIdentifierIndex name, MethodListIndex meths), ClassExtensionIndex> {
    static constexpr auto name { "ClassExtension" };
};

struct ClassOrExtensionList : priv::ListNode<ClassOrExtensionList, ClassOrExtensionListIndex, ClassOrExtensionIndex> {
    static constexpr auto name { "ClassOrExtensionList" };
};

struct RegionList : priv::ListNode<RegionList, RegionListIndex, error_index<ExprSeqIndex>> {
    static constexpr auto name { "RegionList" };
};

struct Error : priv::ListNode<Error, ErrorIndex, AnyIndex> {
    static constexpr auto name { "Error" };
};

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

using NodeCollection = priv::IRNodeCollectionHelper<
    Missing, ASCIINode, IntNode, FloatNode, PiNode, AccidentalNode, StringLineNode, StringLineList, SymbolNode,
    BooleanNode, NilNode, CurryNode, BlockList, ArrayNode, NamedIdentifier, PrimitiveIdentifier, ClassNameIdentifier,
    EnvIdentifierNode, SelectorNode, SelectorWAdverb, VariadicArgNode, ArgumentList, BlockNode, KwArgNode, ExprSeq,
    MessageNode, FunctionNode, AdverbExprNode, ReferenceNode, AssignmentNode, AssignmentAtNode, SetterNode,
    DictionaryNode, DictionaryEntryNode, CollectionNode, DeclareArgumentWithDefaultNode, DeclareArgumentVariadicNode,
    DeclareArgumentList, DeclareVariableWithDefaultNode, DeclareVariableList, BlockContentsList, NonLocalReturnExpr,
    Method, ClassMethod, Class, ClassExtension, ClassAnyVarList, DeclareMemberList, DeclareClassMemberList,
    DeclareConstList, DeclareClassVar, MethodList, ClassOrExtensionList, RegionList, Error>;

using NodeVariant = NodeCollection::variant;
// Useful for meta programming
using NodeTuple = NodeCollection::tuple;

static_assert(NodeCollection::has_node<MessageNode::IndexType>());
static_assert(NodeCollection::has_node<KwArgNode::IndexType>());
};
