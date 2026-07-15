// Copyright Jordan Henderson 2026
#pragma once

namespace sc::parser {

enum struct NodeFlag {
    Missing,
    // parts of lits
    StringLineLiteral,
    // lits
    IntegerLiteral,
    FloatLiteral,
    PiLiteral,
    CurryLiteral,
    AccidentalLiteral,
    StringLiteral,
    FunctionLiteral,
    ArrayLiteral,
    SymbolLiteral,
    SelectorLiteral,
    BooleanLiteral,
    NilLiteral,
    // identifiers
    ClassNameIdentifier,
    EnvironmentIdentifier,
    PrimitiveNameIdentifier,
    NameIdentifier, // args, vars, consts (anything lowercase)
    //
    Decl,
    DeclInitialisation,
    //
    KwArg,
    // argument packs
    // There is no BlockArgumentPack, it is just another positional argument pack.
    PositionalArgumentPack,
    KeywordArgumentPack,
    VariadicArgumentPack,
    //
    ArgumentList,
    //
    MessageCall,


    //
    NonLocalReturnExpr,
    Expr,
    MethodExpr,
    //
    MethodDefinition,
    ClassDefinition,
    // lists
    ExprList,
    KwArgsList,
    MethodDefList,
    ClassDefList,
    DeclList
};
}
