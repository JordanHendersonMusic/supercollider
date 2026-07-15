#pragma once
#include "typed_index.hpp"
#include <cstdint>

namespace sc::parser {

// Elements the indexs can represent.
// The types of node in the graph.
enum struct NodeFlag {
    // Misc
    Missing,
    Error,

    // parts of lits
    StringLineLiteral,

    // lits
    IntegerLiteral,
    FloatLiteral,
    PiLiteral,
    CurryLiteral,
    ASCIILiteral,
    AccidentalLiteral,
    StringLiteral,
    FunctionLiteral,
    ArrayLiteral,
    SymbolLiteral,
    SelectorLiteral,
    SelectorWAdverb,
    BooleanLiteral,
    NilLiteral,
    BlockLiteral,
    BlockList,
    Array,
    Dictionary,
    DictionaryEntry,
    Collection,

    // identifiers
    ClassNameIdentifier,
    EnvIdentifier,
    PrimitiveNameIdentifier,
    NameIdentifier, // args, vars, consts (anything lowercase)

    // argument calling
    KwArg,
    VariadicArgument,

    // when calling argument
    ArgumentList,

    // This is the heart of the expr.
    ExprSeq,

    // expr stuff
    MessageCall,
    AdverbExpr,
    Reference,
    Assignment,
    AssignmentAt,
    Setter,
    NonLocalReturnExpr,

    // defining functions/methods
    DeclareArgumentVariadic,
    DeclareArgumentWithDefault,
    DeclareArgumentList,
    //
    DeclareVariableWithDefault,
    DeclareVariableList,
    //
    BlockContentsList, // stuff that goes in a function.

    // methods
    Method,
    ClassMethod,
    MethodContentsList,
    MethodList,

    // class instances variables
    DeclareClassVar,

    DeclareMemberList,
    DeclareClassMemberList,
    DeclareConstList,


    DeclareClassAnyVarList,


    //
    Class,
    ClassExtension,
    //
    ClassOrExtensionList,

    RegionList,

    COUNT
};


namespace details {
using Spec = sc::util::typed_index::Spec<std::uint32_t, NodeFlag, struct ASTGraph>;
using Def = sc::util::typed_index::QuicklyDefineTypesFromSpec<Spec>;
}

using Index = details::Def::Index;
using OptionalIndex = details::Def::OptionalIndex;

// Because bison requires that the indexs be default constructable, we must use the optional index type here.
template <NodeFlag... Flags> using TypedIndex = details::Def::OptionalTypedIndex<Flags...>;

template <typename... Is> using join = sc::util::typed_index::join<Is...>;

template <typename T> using maybe = join<T, TypedIndex<NodeFlag::Missing>>;

template <typename T> using error_index = join<T, TypedIndex<NodeFlag::Error>>;
}
