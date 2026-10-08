// Copyright Jordan Henderson 2026
#pragma once
#include "index.hpp"

namespace sc::ast {

using MissingIndex = TypedIndex<NodeFlag::Missing>;
using ErrorIndex = TypedIndex<NodeFlag::Error>;

using StringLineLitIndex = TypedIndex<NodeFlag::StringLineLiteral>;

// Literal indexes
using IntLitIndex = TypedIndex<NodeFlag::IntegerLiteral>;
using FloatLitIndex = TypedIndex<NodeFlag::FloatLiteral>;

// A literal number, int or float
using NumberIndex = join<IntLitIndex, FloatLitIndex>;

using PiLitIndex = TypedIndex<NodeFlag::PiLiteral>;
using CurryIndex = TypedIndex<NodeFlag::CurryLiteral>;
using ASCIIIndex = TypedIndex<NodeFlag::ASCIILiteral>;
using AccidentalIndex = TypedIndex<NodeFlag::AccidentalLiteral>;

using FloatProducingIndex = join<FloatLitIndex, PiLitIndex, AccidentalIndex>;

using StringLitIndex = TypedIndex<NodeFlag::StringLiteral>;

using SymbolLitIndex = TypedIndex<NodeFlag::SymbolLiteral>;

using BooleanLitIndex = TypedIndex<NodeFlag::BooleanLiteral>;
using NilLitIndex = TypedIndex<NodeFlag::NilLiteral>;

using ArrayIndex = TypedIndex<NodeFlag::Array>;
using DictionaryEntryIndex = TypedIndex<NodeFlag::DictionaryEntry>;
using DictionaryIndex = TypedIndex<NodeFlag::Dictionary>;
using CollectionIndex = TypedIndex<NodeFlag::Collection>;

using BlockIndex = TypedIndex<NodeFlag::BlockLiteral>;
using BlockListIndex = TypedIndex<NodeFlag::BlockList>;

using AnyDefiniteLiteralIndex =
    join<ASCIIIndex, IntLitIndex, FloatProducingIndex, StringLitIndex, BooleanLitIndex, NilLitIndex, SymbolLitIndex>;

using AnyPossiblyLiteralIndex =
    join<ASCIIIndex, IntLitIndex, FloatProducingIndex, StringLitIndex, BooleanLitIndex, NilLitIndex, SymbolLitIndex,
         BlockIndex, ArrayIndex, DictionaryIndex, CollectionIndex>;

using ClassNameIdentifierIndex = TypedIndex<NodeFlag::ClassNameIdentifier>;
using NamedIdentifierIndex = TypedIndex<NodeFlag::NameIdentifier>;
using EnvIdentifierIndex = TypedIndex<NodeFlag::EnvIdentifier>;
using PrimitiveIdentifierIndex = TypedIndex<NodeFlag::PrimitiveNameIdentifier>;

// TODO: remove SelectorLiteral
using SelectorIndex = join<TypedIndex<NodeFlag::SelectorLiteral>, NamedIdentifierIndex>;
using SelectorWAdverbIndex = TypedIndex<NodeFlag::SelectorWAdverb>;

using SelectorMaybeAdverbIndex = join<SelectorIndex, SelectorWAdverbIndex>;

using MessageIndex = TypedIndex<NodeFlag::MessageCall>;

using ReferenceIndex = TypedIndex<NodeFlag::Reference>;
using AssignmentIndex = TypedIndex<NodeFlag::Assignment>;
using AssignmentAtIndex = TypedIndex<NodeFlag::AssignmentAt>;
using SetterIndex = TypedIndex<NodeFlag::Setter>;

using NonLocalReturnExprIndex = TypedIndex<NodeFlag::NonLocalReturnExpr>;

// This is a little confusing, but an ExprSeqIndex can be a list of expression, or just a single expr.
// We use ExprSeqIndex rather than create an ExprSeq because from the compiler's point, they are the same, and we get to
//      save an extra node.
using ExprSeqIndex = TypedIndex<NodeFlag::ExprSeq>;

using AnyExprIndex =
    join<ExprSeqIndex, AnyPossiblyLiteralIndex, ClassNameIdentifierIndex, EnvIdentifierIndex, NamedIdentifierIndex,
         MessageIndex, NonLocalReturnExprIndex, ReferenceIndex, AssignmentIndex, AssignmentAtIndex, SetterIndex>;


using AdverbExprIndex = TypedIndex<NodeFlag::AdverbExpr>;
using AdverbIndex = TypedIndex<NodeFlag::AdverbExpr, NodeFlag::IntegerLiteral, NodeFlag::NameIdentifier>;

using KwArgIndex = TypedIndex<NodeFlag::KwArg>;
using VariadicArgIndex = TypedIndex<NodeFlag::VariadicArgument>;

using ArgumentEntryIndex = join<AnyExprIndex, KwArgIndex, VariadicArgIndex, BlockListIndex>;

using ArgumentListIndex = TypedIndex<NodeFlag::ArgumentList>;

// Declare arguments

using DeclareArgumentVariadicIndex = TypedIndex<NodeFlag::DeclareArgumentVariadic>;
using DeclareArgumentWithDefaultIndex = TypedIndex<NodeFlag::DeclareArgumentWithDefault>;

using DeclareAnyArgumentIndex =
    join<NamedIdentifierIndex, DeclareArgumentWithDefaultIndex, DeclareArgumentVariadicIndex>;

using DeclareArgumentListIndex = TypedIndex<NodeFlag::DeclareArgumentList>;

// Declare variables

using DeclareVariableWithDefaultIndex = TypedIndex<NodeFlag::DeclareVariableWithDefault>;

using DeclareAnyVariableIndex = join<NamedIdentifierIndex, DeclareVariableWithDefaultIndex>;

using DeclareVariableListIndex = TypedIndex<NodeFlag::DeclareVariableList>;

using BlockItemIndex = join<AnyExprIndex, DeclareVariableListIndex>;
using BlockContentsListIndex = TypedIndex<NodeFlag::BlockContentsList>;

using MethodContentsListIndex = TypedIndex<NodeFlag::MethodContentsList>;
using MethodIndex = TypedIndex<NodeFlag::Method>;
using ClassMethodIndex = TypedIndex<NodeFlag::ClassMethod>;
using AnyMethodIndex = join<MethodIndex, ClassMethodIndex, TypedIndex<NodeFlag::Error>>;

using MethodListIndex = TypedIndex<NodeFlag::MethodList>;

using DeclareClassVarIndex = TypedIndex<NodeFlag::DeclareClassVar>;

using DeclareMemberListIndex = TypedIndex<NodeFlag::DeclareMemberList>;
using DeclareClassMemberListIndex = TypedIndex<NodeFlag::DeclareClassMemberList>;
using DeclareConstListIndex = TypedIndex<NodeFlag::DeclareConstList>;

using DeclareAnyList = join<DeclareMemberListIndex, DeclareClassMemberListIndex, DeclareConstListIndex>;

using DeclareClassAnyVarListIndex = TypedIndex<NodeFlag::DeclareClassAnyVarList>;

using ClassIndex = TypedIndex<NodeFlag::Class>;
using ClassExtensionIndex = TypedIndex<NodeFlag::ClassExtension>;
using ClassOrExtensionIndex = join<ClassIndex, ClassExtensionIndex>;

using ClassOrExtensionListIndex = TypedIndex<NodeFlag::ClassOrExtensionList>;

using RegionListIndex = TypedIndex<NodeFlag::RegionList>;
using ClassListOrExprListIndex = join<RegionListIndex, ExprSeqIndex, ClassOrExtensionListIndex>;

template <typename T> struct Wrapper {
    using Type = T;
};

template <std::size_t... I> auto any_index_builder(std::index_sequence<I...>) {
    using Type = TypedIndex<static_cast<NodeFlag>(I)...>;
    return Wrapper<Type> { };
};

// Accepts any index, without resorting to using Index or OptionalIndex directly.
using AnyIndex = typename decltype(any_index_builder(std::index_sequence<static_cast<int>(NodeFlag::COUNT)> { }))::Type;
}
