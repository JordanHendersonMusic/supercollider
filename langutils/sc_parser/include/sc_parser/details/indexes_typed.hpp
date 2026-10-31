// Copyright Jordan Henderson 2026
#pragma once

#include "sc_util/type_set_index.hpp"
#include "sc_parser/details/indexes_untyped.hpp"


namespace sc::ast {

/// @brief An index to any node, which may be empty.
using UntypedIndex = OptionalIndex;

/// An index that can only refer to nodes of the given types.
/// Because bison requires that the indexes be default constructable, this is the optional index type.
/// @tparam Flags The node types it may refer to.
template <NodeFlag... Flags> using TypedIndex = details::Def::OptionalTypedIndex<Flags...>;

/// The index that may refer to any node type that one of @p Is may refer to.
template <typename... Is> using join = sc::util::typed_index::join<Is...>;

/// @p T, or a Missing node when the thing is optional in the grammar.
template <typename T> using maybe = join<T, TypedIndex<NodeFlag::Missing>>;

/// @p T, or an Error node where the parser recovered from a syntax error.
template <typename T> using error_index = join<T, TypedIndex<NodeFlag::Error>>;


/// @brief A node marking something optional that was not written.
using MissingIndex = TypedIndex<NodeFlag::Missing>;
/// @brief A node holding a syntax error the parser recovered from.
using ErrorIndex = TypedIndex<NodeFlag::Error>;

/// @brief One line of a string literal.
using StringLineLitIndex = TypedIndex<NodeFlag::StringLineLiteral>;

// Literal indexes
/// @brief An integer literal.
using IntLitIndex = TypedIndex<NodeFlag::IntegerLiteral>;
/// @brief A float literal.
using FloatLitIndex = TypedIndex<NodeFlag::FloatLiteral>;

/// @brief A literal number, int or float.
using NumberIndex = join<IntLitIndex, FloatLitIndex>;

/// @brief The constant `pi`, possibly multiplied by a number.
using PiLitIndex = TypedIndex<NodeFlag::PiLiteral>;
/// @brief The `_` placeholder of a partial application.
using CurryIndex = TypedIndex<NodeFlag::CurryLiteral>;
/// @brief A character literal, e.g., `$a`.
using ASCIIIndex = TypedIndex<NodeFlag::ASCIILiteral>;
/// @brief An accidental literal, in steps or in cents.
using AccidentalIndex = TypedIndex<NodeFlag::AccidentalLiteral>;

/// @brief Literals that evaluate to a float.
using FloatProducingIndex = join<FloatLitIndex, PiLitIndex, AccidentalIndex>;

/// @brief A string literal, made of one or more lines.
using StringLitIndex = TypedIndex<NodeFlag::StringLiteral>;


/// @brief A keyword, such as the `key:` in `f(key: 1)`.
using KeyBinOpIndex = TypedIndex<NodeFlag::KeybinOp>;
/// @brief A symbol written in quotes, e.g., `'symbol'`.
using SymbolQuoteIndex = TypedIndex<NodeFlag::SymbolQuote>;
/// @brief A symbol written with a slash, e.g., `\\symbol`.
using SymbolSlashIndex = TypedIndex<NodeFlag::SymbolSlash>;

/// @brief Either kind of symbol.
using AnySymbolIndex = join<SymbolQuoteIndex, SymbolSlashIndex>;

/// @brief `true` or `false`.
using BooleanLitIndex = TypedIndex<NodeFlag::BooleanLiteral>;
/// @brief `nil`.
using NilLitIndex = TypedIndex<NodeFlag::NilLiteral>;

/// @brief A range in an index, e.g., `a[1..3]`.
using RangeIndex = TypedIndex<NodeFlag::Range>;
/// @brief A series generator, e.g., `(1, 3..9)`.
using RangeGeneratorIndex = TypedIndex<NodeFlag::RangeGenerator>;
/// @brief Assigning to a range, e.g., `a[1..3] = x`.
using RangeAssignIndex = TypedIndex<NodeFlag::RangeAssign>;

/// @brief An array literal, e.g., `[1, 2]`.
using ArrayIndex = TypedIndex<NodeFlag::Array>;
/// @brief One key and value pair in a dictionary.
using DictionaryEntryIndex = TypedIndex<NodeFlag::DictionaryEntry>;
/// @brief A dictionary literal, e.g., `(a: 1, b: 2)`.
using DictionaryIndex = TypedIndex<NodeFlag::Dictionary>;
/// @brief A class name followed by an array, e.g., `Set[1, 2]`.
using CollectionIndex = TypedIndex<NodeFlag::Collection>;

/// @brief A function literal, e.g., `{ |x| x }`.
using BlockIndex = TypedIndex<NodeFlag::BlockLiteral>;
/// @brief The functions and generators written after the arguments of a message, e.g., `f(1) { } { }`.
using BlockListIndex = TypedIndex<NodeFlag::BlockList>;

/// @brief A parenthesised group of statements in interpreted code, e.g., `( var a = 1; a + 1 )`.
using ImmediatelyEvaluatedBlockIndex = TypedIndex<NodeFlag::ImmediatelyEvaluatedBlock>;
/// @brief Literals that are always literals.
using AnyDefiniteLiteralIndex =
    join<ASCIIIndex, IntLitIndex, FloatProducingIndex, StringLitIndex, BooleanLitIndex, NilLitIndex, AnySymbolIndex>;

/// @brief Things that may be literals, depending on what they contain.
using AnyPossiblyLiteralIndex = join<AnyDefiniteLiteralIndex, BlockIndex, ArrayIndex, DictionaryIndex, CollectionIndex>;

/// @brief A class name.
using ClassNameIdentifierIndex = TypedIndex<NodeFlag::ClassNameIdentifier>;

/// @brief Expressions whose value is always known at compile time. Class names aren't literals, but are constants.
using AnyDefiniteConstantIndex = join<AnyDefiniteLiteralIndex, ClassNameIdentifierIndex>;

/// @brief A lower case name: an argument, variable, constant, or method name.
using NamedIdentifierIndex = TypedIndex<NodeFlag::NameIdentifier>;
/// @brief An environment variable, e.g., `~name`.
using EnvIdentifierIndex = TypedIndex<NodeFlag::EnvIdentifier>;
/// @brief A primitive name, e.g., `_Primitive`.
using PrimitiveIdentifierIndex = TypedIndex<NodeFlag::PrimitiveNameIdentifier>;

/// @brief A name or an environment variable.
using NameOrEnvNameIndex = join<NamedIdentifierIndex, EnvIdentifierIndex>;

/// @brief A list of names.
using NameListIndex = TypedIndex<NodeFlag::NameList>;
/// @brief Names unpacked from a value, e.g., `#a, b, c`.
using NamePackIndex = TypedIndex<NodeFlag::NamePack>;

// TODO: remove SelectorLiteral
/// @brief The name of a message.
using SelectorIndex = join<TypedIndex<NodeFlag::SelectorLiteral>, NamedIdentifierIndex>;
/// @brief A selector followed by an adverb.
using SelectorWAdverbIndex = TypedIndex<NodeFlag::SelectorWAdverb>;

/// @brief A selector, with or without an adverb.
using SelectorMaybeAdverbIndex = join<SelectorIndex, SelectorWAdverbIndex>;


/// @brief A reference, e.g., `` `x ``.
using ReferenceIndex = TypedIndex<NodeFlag::Reference>;
/// @brief An assignment, e.g., `x = 1`.
using AssignmentIndex = TypedIndex<NodeFlag::Assignment>;
/// @brief An indexed assignment, e.g., `a[0] = 1`.
using AssignmentAtIndex = TypedIndex<NodeFlag::AssignmentAt>;
/// @brief A setter, e.g., `a.x = 1`.
using SetterIndex = TypedIndex<NodeFlag::Setter>;

/// @brief A non local return, e.g., `^1`.
using NonLocalReturnExprIndex = TypedIndex<NodeFlag::NonLocalReturnExpr>;

/// @brief A sequence of expressions. This is a little confusing, but an ExprSeqIndex can be a list of expressions, or
/// just a single expression. We use ExprSeqIndex rather than create an ExprSeq because from the compiler's point they
/// are the same, and we get to save an extra node.
using ExprSeqIndex = TypedIndex<NodeFlag::ExprSeq>;


/// @brief A list comprehension, e.g., `{: x * 2, x <- list }`.
using GeneratorIndex = TypedIndex<NodeFlag::Generator>;
/// @brief Something written like a generator but with `;` in place of `:`, e.g., `{; x, x <- list }`.
using GeneratorFakeIndex = TypedIndex<NodeFlag::GeneratorFake>;

/// @brief Either kind of generator.
using GeneratorAnyIndex = join<GeneratorIndex, GeneratorFakeIndex>;

/// @brief A generator qualifier that draws values from a collection, e.g., `x <- list`, or `x i <- list` to also name
/// the index.
using GeneratorQualAssignIndex = TypedIndex<NodeFlag::GeneratorQualAssign>;
/// @brief A generator qualifier that declares a name, e.g., `var y = x`.
using GeneratorQualDeclareIndex = TypedIndex<NodeFlag::GeneratorQualDeclare>;
/// @brief A generator qualifier that is a bare expression, which filters, e.g., `x > 1`.
using GeneratorQualIfIndex = TypedIndex<NodeFlag::GeneratorQualIf>;
/// @brief A generator qualifier that runs an expression for its side effect, e.g., `:: x.postln`.
using GeneratorQualSideEffectIndex = TypedIndex<NodeFlag::GeneratorQualSideEffect>;
/// @brief A generator qualifier that stops the generator, e.g., `:while x < 10`.
using GeneratorQualGuardIndex = TypedIndex<NodeFlag::GeneratorQualGuard>;

/// @brief Any generator qualifier.
using GeneratorQualAnyIndex = join< //
    GeneratorQualAssignIndex, //
    GeneratorQualDeclareIndex, //
    GeneratorQualIfIndex, //
    GeneratorQualSideEffectIndex, //
    GeneratorQualGuardIndex //
    >;

/// @brief The qualifiers of a generator.
using GeneratorQualListIndex = TypedIndex<NodeFlag::GeneratorQualList>;


/// @brief A message written without a receiver, e.g., `f(1)`.
using PrefixMessageIndex = TypedIndex<NodeFlag::PrefixMessage>;
/// @brief A message sent to a receiver, e.g., `a.f(1)`.
using PostfixMessageIndex = TypedIndex<NodeFlag::PostfixMessage>;
/// @brief A binary operator, e.g., `a + b`.
using BinaryMessageIndex = TypedIndex<NodeFlag::BinaryMessage>;
/// @brief A call of a value, e.g., `f.(1)`.
using ValueMessageIndex = TypedIndex<NodeFlag::ValueMessage>;
/// @brief A constructor call, e.g., `Foo(1)`.
using NewMessageIndex = TypedIndex<NodeFlag::NewMessage>;
/// @brief An index, e.g., `a[0]`.
using AtMessageIndex = TypedIndex<NodeFlag::AtMessage>;

/// @brief Any message.
using AnyMessageIndex = join< //
    PrefixMessageIndex, //
    PostfixMessageIndex, //
    BinaryMessageIndex, //
    ValueMessageIndex, //
    NewMessageIndex, //
    AtMessageIndex //
    >;


/// @brief Declaring several variables at once from a value, e.g., `var #a, b = x`.
using VariablePackAssignIndex = TypedIndex<NodeFlag::VariablePackAssign>;
/// @brief Any expression.
/// Includes errors, as the parser can recover from an error in an expression and carry on, e.g., `(1 +)`.
using AnyExprIndex = join< //
    ErrorIndex, //
    ExprSeqIndex, //
    AnyPossiblyLiteralIndex, //
    ClassNameIdentifierIndex, //
    EnvIdentifierIndex, //
    NamedIdentifierIndex, //
    AnyMessageIndex, //
    NonLocalReturnExprIndex, //
    ReferenceIndex, //
    AssignmentIndex, //
    AssignmentAtIndex, //
    SetterIndex, //
    VariablePackAssignIndex, //
    GeneratorAnyIndex, //
    RangeIndex, //
    RangeGeneratorIndex, //
    RangeAssignIndex, //
    CurryIndex //
    >;

static_assert(!std::is_convertible_v<KeyBinOpIndex, AnyExprIndex>);

/// @brief An adverb that is an expression.
using AdverbExprIndex = TypedIndex<NodeFlag::AdverbExpr>;
/// @brief An adverb: an expression, an integer, or a name.
using AdverbIndex = TypedIndex<NodeFlag::AdverbExpr, NodeFlag::IntegerLiteral, NodeFlag::NameIdentifier>;

/// @brief A keyword argument, e.g., `key: value`.
using KwArgIndex = TypedIndex<NodeFlag::KwArg>;
/// @brief A variadic argument at a call, e.g., `*list`.
using VariadicArgIndex = TypedIndex<NodeFlag::VariadicArgument>;

/// @brief Anything that can appear in the arguments of a call.
using ArgumentEntryIndex = join<AnyExprIndex, KwArgIndex, VariadicArgIndex, BlockListIndex>;

/// @brief The arguments of a call.
using ArgumentListIndex = TypedIndex<NodeFlag::ArgumentList>;


/// @brief What can appear between the brackets of an index.
using AtMessageArgIndex = join<RangeIndex, ArgumentListIndex>;

// Declare arguments

/// @brief A variadic argument declaration, e.g., `*rest`.
using DeclareArgumentVariadicIndex = TypedIndex<NodeFlag::DeclareArgumentVariadic>;
/// @brief An argument declaration with a default, e.g., `x = 1`.
using DeclareArgumentWithDefaultIndex = TypedIndex<NodeFlag::DeclareArgumentWithDefault>;

/// @brief Any argument declaration.
using DeclareAnyArgumentIndex =
    join<NamedIdentifierIndex, DeclareArgumentWithDefaultIndex, DeclareArgumentVariadicIndex>;

/// @brief The arguments a function or method declares.
using DeclareArgumentListIndex = TypedIndex<NodeFlag::DeclareArgumentList>;

// Declare variables


/// @brief A variable declaration with a value, e.g., `var x = 1`.
using DeclareVariableWithDefaultIndex = TypedIndex<NodeFlag::DeclareVariableWithDefault>;

/// @brief Any variable declaration.
using DeclareAnyVariableIndex = join<NamedIdentifierIndex, DeclareVariableWithDefaultIndex, VariablePackAssignIndex>;

/// @brief One `var` statement, holding one or more declarations.
using DeclareVariableListIndex = TypedIndex<NodeFlag::DeclareVariableList>;

/// @brief Anything that can appear in the body of a function.
using BlockItemIndex = join<AnyExprIndex, DeclareVariableListIndex>;
/// @brief The body of a function.
using BlockContentsListIndex = TypedIndex<NodeFlag::BlockContentsList>;

/// @brief The body of a method.
using MethodContentsListIndex = TypedIndex<NodeFlag::MethodContentsList>;
/// @brief An instance method.
using MethodIndex = TypedIndex<NodeFlag::Method>;
/// @brief A class method, written `*name`.
using ClassMethodIndex = TypedIndex<NodeFlag::ClassMethod>;
/// @brief Any method, or an error where the parser recovered.
using AnyMethodIndex = join<MethodIndex, ClassMethodIndex, TypedIndex<NodeFlag::Error>>;
/// @brief Any method, never an error.
using AnyValidMethodIndex = join<MethodIndex, ClassMethodIndex>;

/// @brief The methods of a class or extension.
using MethodListIndex = TypedIndex<NodeFlag::MethodList>;

/// @brief One declared instance variable, class variable, or constant.
using DeclareMemberIndex = TypedIndex<NodeFlag::DeclareMember>;
/// @brief One declared constant.
using DeclareConstMemberIndex = TypedIndex<NodeFlag::DeclareConst>;

/// @brief The `<`, `>`, or `<>` before a declared member.
using RWAccessorIndex = TypedIndex<NodeFlag::RWAccessor>;
/// @brief An accessor, or Missing if there is none.
using MaybeRWAccessorIndex = maybe<RWAccessorIndex>;

/// @brief An instance variable statement, `var`.
using DeclareMemberListIndex = TypedIndex<NodeFlag::DeclareMemberList>;
/// @brief A class variable statement, `classvar`.
using DeclareClassMemberListIndex = TypedIndex<NodeFlag::DeclareClassMemberList>;
/// @brief A constant statement, `const`.
using DeclareConstListIndex = TypedIndex<NodeFlag::DeclareConstList>;

/// @brief A syntax error in a class's members is kept as an Error, see error_recovery::member.
using BigMemberEntryIndex =
    join<DeclareMemberListIndex, DeclareClassMemberListIndex, DeclareConstListIndex, ErrorIndex>;

/// @brief All the variable and constant statements of a class.
using BigMemberListIndex = TypedIndex<NodeFlag::BigMemberList>;

/// @brief A class definition.
using ClassIndex = TypedIndex<NodeFlag::Class>;
/// @brief A class extension, e.g., `+ Foo { ... }`.
using ClassExtensionIndex = TypedIndex<NodeFlag::ClassExtension>;
/// @brief A class or an extension.
using ClassOrExtensionIndex = join<ClassIndex, ClassExtensionIndex>;

/// @brief A file of classes and extensions.
using ClassOrExtensionListIndex = TypedIndex<NodeFlag::ClassOrExtensionList>;

/// @brief A file written as a function.
using FileAsFunctionIndex = TypedIndex<NodeFlag::FileAsFunction>;

/// @brief One item of interpreted code: an expression, an immediately evaluated block, or an error.
using RegionItemIndex = join<ErrorIndex, AnyExprIndex, ImmediatelyEvaluatedBlockIndex>;
/// @brief Interpreted code: a list of its items.
using RegionListIndex = TypedIndex<NodeFlag::RegionList>;

/// @brief What the parser can produce at the root of the graph.
using GoIndex = join<RegionListIndex, ClassOrExtensionListIndex, FileAsFunctionIndex>;

template <typename T> struct Wrapper { using Type = T; };

template <std::size_t... I> auto any_index_builder(std::index_sequence<I...>) {
    using Type = TypedIndex<static_cast<NodeFlag>(I)...>;
    return Wrapper<Type> {};
};

/// @brief Accepts any index, without resorting to using Index or OptionalIndex directly.
using AnyIndex = typename decltype(any_index_builder(std::index_sequence<static_cast<int>(NodeFlag::COUNT)> {}))::Type;
}
