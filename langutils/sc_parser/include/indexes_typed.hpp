// Copyright Jordan Henderson 2026
#pragma once
#include "index.hpp"
#include "node_flags.hpp"

// See documentation in ir_nodes.hpp

namespace sc::parser {


using MissingIndex = TypedIndex<NodeFlag::Missing>;

// Literal indexes
using IntLitIndex = TypedIndex<NodeFlag::IntegerLiteral>;
using FloatLitIndex = TypedIndex<NodeFlag::FloatLiteral>;

// A literal number, int or float
using NumberIndex = join<IntLitIndex, FloatLitIndex>;

using PiLitIndex = TypedIndex<NodeFlag::PiLiteral>;

using AccidentalIndex = TypedIndex<NodeFlag::AccidentalLiteral>;

using FloatProducingIndex = join<FloatLitIndex, PiLitIndex, AccidentalIndex>;

using CurryIndex = TypedIndex<NodeFlag::CurryLiteral>;

using AccidentalLitIndex = TypedIndex<NodeFlag::AccidentalLiteral>;

using StringLineLitIndex = TypedIndex<NodeFlag::StringLineLiteral>;
using StringLitIndex = TypedIndex<NodeFlag::StringLiteral>;

using FunctionLitIndex = TypedIndex<NodeFlag::FunctionLiteral>;

using ArrayLitIndex = TypedIndex<NodeFlag::ArrayLiteral>;

using SymbolLitIndex = TypedIndex<NodeFlag::SymbolLiteral>;

using BooleanLitIndex = TypedIndex<NodeFlag::BooleanLiteral>;

using NilLitIndex = TypedIndex<NodeFlag::NilLiteral>;

using AnyLiteralIndex = join<IntLitIndex, FloatProducingIndex, StringLitIndex, FunctionLitIndex, ArrayLitIndex,
                             BooleanLitIndex, NilLitIndex, SymbolLitIndex>;

using SelectorIndex = TypedIndex<NodeFlag::SelectorLiteral>;

// Names of stuff
using ClassNameIdentifierIndex = TypedIndex<NodeFlag::ClassNameIdentifier>;
using EnvIdentifierIndex = TypedIndex<NodeFlag::EnvironmentIdentifier>;
using NameIndex = TypedIndex<NodeFlag::NameIdentifier>;
using PrimitiveIdentifierIndex = TypedIndex<NodeFlag::PrimitiveNameIdentifier>;


// This does most of the heavy lifting.
using MessageIndex = TypedIndex<NodeFlag::MessageCall>;

using DeclIndex = TypedIndex<NodeFlag::Decl>;
using DeclInitialisationIndex = TypedIndex<NodeFlag::DeclInitialisation>;

using NonLocalReturnExprIndex = TypedIndex<NodeFlag::NonLocalReturnExpr>;

using ExprListIndex = TypedIndex<NodeFlag::ExprList>;
using ExprIndex = join<TypedIndex<NodeFlag::Expr>, AnyLiteralIndex, ClassNameIdentifierIndex, EnvIdentifierIndex,
                       NameIndex, MessageIndex, DeclIndex, NonLocalReturnExprIndex>;

using KwArgIndex = TypedIndex<NodeFlag::KwArg>;

// ArgumentPacks
using PositionalArgumentPackIndex = TypedIndex<NodeFlag::PositionalArgumentPack>;
using KeywordArgumentPackIndex = TypedIndex<NodeFlag::KeywordArgumentPack>;
using VariadicArgumentPackIndex = TypedIndex<NodeFlag::VariadicArgumentPack>;

using ArgumentEntryIndex = join<PositionalArgumentPackIndex, KeywordArgumentPackIndex, VariadicArgumentPackIndex>;
using ArgumentListIndex = TypedIndex<NodeFlag::ArgumentList>;


using ArgumentAnyIndex = join<ArgumentListIndex, ArgumentEntryIndex, ExprIndex>;

using MethodDefIndex = TypedIndex<NodeFlag::MethodDefinition>;
using ClassDefIndex = TypedIndex<NodeFlag::ClassDefinition>;


static_assert([]() -> bool {
    const IntLitIndex i { 2 };
    const ExprIndex e { i }; // << try replacing this with something else, it ought not to compile
    return *e == 2;
}());

static_assert([]() -> bool {
    IntLitIndex i { 2 };
    ExprIndex e { i };
    return *e == 2;
}());

static_assert([]() -> bool {
    ExprIndex e1 { 2 };
    ExprIndex e2 { e1 };
    return *e2 == 2;
}());

static_assert([]() {
    MissingIndex e { 3 };
    MaybeIndex i { *e };
    return static_cast<bool>(i);
}());
}
