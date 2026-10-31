#pragma once

#include "sc_util/language_version.hpp"
#include "sc_util/details/typed_graph_node.hpp"

#include "sc_parser/details/read_write_accessor.hpp"

#include "sc_sema/details/source_location.hpp"
#include "sc_sema/details/ir_nodes_details.hpp"
#include "sc_sema/details/indexes.hpp"
#include "sc_sema/details/symbols.hpp"
#include "sc_sema/details/type_info.hpp"


namespace sc::ir {

namespace priv {
template <typename Index> using Terminal = sc::util::typed_graph::TerminalNode<Index>;
template <typename This, typename... ARGS> using Node = sc::util::typed_graph::Node<This, ARGS...>;
template <typename This, typename Child> using List = sc::util::typed_graph::ListNode<This, Child>;
}

////////////////////////////////////////////////////////////////////////////////

/// @brief An optional child that is not there.
struct SCMissing : priv::Terminal<SCMissing_I> {
    static constexpr auto type_name { "SCMissing" };
};

/// @brief A literal. This is just a place holder, you need to look the value up in the ConstantTable.
struct SCLiteral : priv::Terminal<SCLiteral_I> {
    static constexpr auto type_name { "SCLiteral" };
    ConstExprIndex index; ///< The literal's value in the ConstantTable.
    constexpr SCLiteral(ConstExprIndex i): index(i) {}
};

/// @brief A keyword argument, e.g., `key: value`. `name` is the keyword.
struct SCKwArg : priv::Node<SCKwArg_I(SCAnyExpr_I value)> {
    static constexpr auto type_name { "SCKwArg" };
    Locatable<SymSCIdentifier> name; ///< The keyword.
    SCKwArg(Locatable<SymSCIdentifier> n): name(n) {}
};

/// @brief A read of a variable. `kind` is where the variable lives, `local` or `member` index it when that applies.
struct SCRead : priv::Terminal<SCRead_I> {
    static constexpr auto type_name { "SCRead" };
    /// @brief Where the variable lives: an argument or `var`, an instance variable, a class variable, or `~name`.
    enum struct Kind { Local, Instance, Class, Environment };
    SymSCIdentifier name; ///< The variable's name.
    Kind kind; ///< Where the variable lives.
    OptionalLocalIndex local; ///< The local, when `kind` is `Local`.
    OptionalMemberIndex member; ///< The member, when `kind` is `Instance` or `Class`.
    SCRead(SymSCIdentifier n, Kind k, OptionalLocalIndex l = {}, OptionalMemberIndex m = {}):
        name(n),
        kind(k),
        local(l),
        member(m) {}
};

/// @brief An assignment to a variable, which evaluates to the assigned value. `kind` is where the variable lives,
/// `local` or `member` index it when that applies.
struct SCAssign : priv::Node<SCAssign_I(SCAnyExpr_I)> {
    static constexpr auto type_name { "SCAssign" };
    /// @brief Where the variable lives: an argument or `var`, an instance variable, a class variable, or `~name`.
    enum struct Kind { Local, Instance, Class, Environment };
    Locatable<SymSCIdentifier> name; ///< The variable's name.
    Kind kind; ///< Where the variable lives.
    OptionalLocalIndex local; ///< The local, when `kind` is `Local`.
    OptionalMemberIndex member; ///< The member, when `kind` is `Instance` or `Class`.
    SCAssign(Locatable<SymSCIdentifier> n, Kind k, OptionalLocalIndex l = {}, OptionalMemberIndex m = {}):
        name(n),
        kind(k),
        local(l),
        member(m) {}
};


/// @brief The positional arguments of a message. `size` is how many there are.
struct SCPositionalArgs : priv::List<SCPosArgList_I, SCAnyExpr_I> {
    static constexpr auto type_name { "SCPositionalArgs" };
    std::uint32_t size {}; ///< How many arguments there are.
};

/// @brief The keyword arguments of a message.
struct SCKwArgs : priv::List<SCKwArgList_I, SCKwArg_I> {
    static constexpr auto type_name { "SCKwArgs" };
};

/// @brief The expressions passed with `*`, whose elements are appended to the positional arguments.
struct SCVariadicArgs : priv::List<SCVariadicArgList_I, SCAnyExpr_I> {
    static constexpr auto type_name { "SCVariadicArgs" };
};

/// @brief A message send. `form` is how it was written: a call, a binary operator, or a setter. `selector` is
/// optionally located, as the compiler can create sends the user did not write.
struct SCMessage : priv::Node<SCMessage_I(SCAnyExpr_I receiver, //
                                          maybe<SCPosArgList_I> positional_arguments, //
                                          maybe<SCKwArgList_I> keyword_arguments, //
                                          maybe<SCVariadicArgList_I> variadic_arguments, //
                                          maybe<SCAnyExpr_I> adverb //
                                          )> {
    static constexpr auto type_name { "SCMessage" };
    /// @brief How the message was written.
    enum struct Form : std::uint8_t { Call, Binary, Setter };
    OptionallyLocatable<SymSCSelector> selector; ///< The message name.
    Form form; ///< How the message was written.
    SCMessage(OptionallyLocatable<SymSCSelector> sel, Form f): selector(sel), form(f) {}
};

/// @brief `var a = 1`, value is missing for `var a`, which is nil.
/// `prototype` is the constant the variable starts as, if its value is one.
struct SCVariableDeclare : priv::Node<SCVariableDeclare_I(maybe<SCAnyExpr_I> value)> {
    static constexpr auto type_name { "SCVariableDeclare" };
    Locatable<SymSCIdentifier> name; ///< The variable's name.
    LocalIndex local; ///< The variable.
    OptionalConstExprIndex prototype; ///< The constant the variable starts as, if its value is one.
    SCVariableDeclare(Locatable<SymSCIdentifier> n, LocalIndex l, OptionalConstExprIndex p):
        name(n),
        local(l),
        prototype(p) {}
};

/// @brief The body of a method or function, its expressions and variable declarations in the order they are written.
struct SCBody : priv::List<SCBody_I, join<SCAnyExpr_I, SCVariableDeclare_I, SCPackDeclare_I>> {
    static constexpr auto type_name { "SCBody" };
};

/// @brief A non local return, e.g., `^value`.
struct SCExplicitReturn : priv::Node<SCExplicitReturn_I(SCAnyExpr_I value)> {
    static constexpr auto type_name { "SCExplicitReturn" };
};

/// @brief A collection, e.g., `[a, b]` or `Set[a, b]`, whose elements are not all constants. `size` is how many
/// elements, and `literal` is whether it was written as a literal.
struct SCCollection : priv::List<SCCollection_I, SCAnyExpr_I> {
    static constexpr auto type_name { "SCCollection" };
    OptionallyLocatable<SymSCClassName> collection_class; ///< The class of the collection.
    std::uint32_t size {}; ///< How many elements there are.
    bool literal { false }; ///< Whether it was written as a literal.
    SCCollection(OptionallyLocatable<SymSCClassName> c): collection_class(c) {}
};


/// @brief `(first..last)` or `(first, second..last)`, a missing second means a step of one or minus one.
/// A missing last means the range is unbounded (to inf), which is only allowed where the range is iterated rather than
/// created, e.g., in a generator.
/// This isn't a message, so later passes can choose how to lower it, e.g., iterating without creating the array.
struct SCRange : priv::Node<SCRange_I(SCAnyExpr_I first, maybe<SCAnyExpr_I> second, maybe<SCAnyExpr_I> last)> {
    static constexpr auto type_name { "SCRange" };
};

/// @brief One name in a pack assignment or declaration, `#a, b ...c = value`. It is assigned value.at(index), or
/// value.copyToEnd(index) when it is the rest, `...c`. The node's location is the name.
struct SCPackTarget : priv::Terminal<SCPackTarget_I> {
    static constexpr auto type_name { "SCPackTarget" };
    Locatable<SymSCIdentifier> name; ///< The variable's name.
    SCAssign::Kind kind; ///< Where the variable lives.
    std::uint32_t index; ///< Which element of the value it is assigned.
    bool rest; ///< Whether it is assigned the rest of the elements, `...c`.
    /// Whether it declares a local in SCPackDeclare or assigns it.
    bool declares { false };
    OptionalLocalIndex local; ///< The local, when `kind` is `Local`.
    OptionalMemberIndex member; ///< The member, when `kind` is `Instance` or `Class`.
    SCPackTarget(Locatable<SymSCIdentifier> n, SCAssign::Kind k, std::uint32_t i, bool r, OptionalLocalIndex l = {},
                 OptionalMemberIndex m = {}):
        name(n),
        kind(k),
        index(i),
        rest(r),
        local(l),
        member(m) {}
};

/// @brief The targets of a pack assignment or declaration.
struct SCPackTargets : priv::List<SCPackTargetList_I, SCPackTarget_I> {
    static constexpr auto type_name { "SCPackTargets" };
};

/// @brief `#a, ~b ...c = value`, evaluates value once and assigns its parts to the targets, see SCPackTarget. Returns
/// value. This isn't lowered to messages, as that needs a temporary to hold value, later passes can choose how to lower
/// it.
struct SCPackAssign : priv::Node<SCPackAssign_I(SCAnyExpr_I value, SCPackTargetList_I targets)> {
    static constexpr auto type_name { "SCPackAssign" };
};

/// @brief `var #a, b ...c = value`, declares each target as a local variable, then assigns it like SCPackAssign.
/// Every target's kind is Local. Like SCVariableDeclare, this isn't an expression.
struct SCPackDeclare : priv::Node<SCPackDeclare_I(SCAnyExpr_I value, SCPackTargetList_I targets)> {
    static constexpr auto type_name { "SCPackDeclare" };
};

////////////////////////////////////////////////////////////////////////////////
// Generators, `{: value, qualifiers }`. Each qualifier nests inside the previous one, as blocks did in the old
// compiler, but they are kept in a flat list in the order they are written. Lowering them, e.g., into a Routine, can
// rebuild the nesting.
// Each qualifier is a block. The variables it declares are a new binding EACH time it iterates.
////////////////////////////////////////////////////////////////////////////////

/// @brief `x <- source` or `x i <- source`, iterates source with do (forSeries for a range), declaring the element, and
/// the index when given, each a new binding per element. source can be an unbounded range, e.g., `x <- (1..)`.
struct SCGenQualIterate : priv::Node<SCGenQualIterate_I(SCAnyExpr_I source)> {
    static constexpr auto type_name { "SCGenQualIterate" };
    Locatable<SymSCIdentifier> element; ///< The element's name.
    LocalIndex element_local; ///< The element's variable.

    std::optional<Locatable<SymSCIdentifier>> index; ///< The index's name, if given.
    OptionalLocalIndex index_local; ///< The index's variable, if given.

    SCGenQualIterate(Locatable<SymSCIdentifier> e, std::optional<Locatable<SymSCIdentifier>> i, LocalIndex el,
                     OptionalLocalIndex il):
        element(e),
        index(i),
        element_local(el),
        index_local(il) {}
};

/// @brief `var x = value`, declares x for the qualifiers after it, a new binding each time it runs.
struct SCGenQualDeclare : priv::Node<SCGenQualDeclare_I(SCAnyExpr_I value)> {
    static constexpr auto type_name { "SCGenQualDeclare" };
    Locatable<SymSCIdentifier> name; ///< The variable's name.
    LocalIndex local; ///< The variable.
    SCGenQualDeclare(Locatable<SymSCIdentifier> n, LocalIndex l): name(n), local(l) {}
};

/// @brief `condition`, the qualifiers after it only run when it is true.
struct SCGenQualIf : priv::Node<SCGenQualIf_I(SCAnyExpr_I condition)> {
    static constexpr auto type_name { "SCGenQualIf" };
};

/// @brief `:: expr`, evaluated for its effects, then the qualifiers after it run.
struct SCGenQualSideEffect : priv::Node<SCGenQualSideEffect_I(SCAnyExpr_I expr)> {
    static constexpr auto type_name { "SCGenQualSideEffect" };
};

/// @brief `:while condition`, ends the whole generator when it is false.
struct SCGenQualWhile : priv::Node<SCGenQualWhile_I(SCAnyExpr_I condition)> {
    static constexpr auto type_name { "SCGenQualWhile" };
};

/// @brief The qualifiers of a generator, in the order they are written.
struct SCGenQualList
    : priv::List<SCGenQualList_I,
                 join<SCGenQualIterate_I, SCGenQualDeclare_I, SCGenQualIf_I, SCGenQualSideEffect_I, SCGenQualWhile_I>> {
    static constexpr auto type_name { "SCGenQualList" };
};


/// @brief `{: value, qualifiers }` makes a Routine, which yields value each time the qualifiers reach it.
struct SCRoutineGenerator : priv::Node<SCRoutineGenerator_I(SCGenQualList_I qualifiers, SCAnyExpr_I value)> {
    static constexpr auto type_name { "SCRoutineGenerator" };
};

/// @brief `{; value, qualifiers }`, the qualifiers run straight away, evaluating value for its effects. It isn't a
/// Routine and has no function of its own, and returns what the outermost qualifier does, e.g., the source of `x <-
/// source`.
struct SCImmediateGenerator : priv::Node<SCImmediateGenerator_I(SCGenQualList_I qualifiers, SCAnyExpr_I value)> {
    static constexpr auto type_name { "SCImmediateGenerator" };
};

/// @brief Like SCRange, but a missing last is allowed, which means it never ends.
struct SCRangeGenerator
    : priv::Node<SCRangeGenerator_I(SCAnyExpr_I first, maybe<SCAnyExpr_I> second, maybe<SCAnyExpr_I> last)> {
    static constexpr auto type_name { "SCRangeGenerator" };
};

/// @brief An argument a method or function declares. `prototype` is the constant it defaults to, if its default is one.
struct SCArgumentDeclare : priv::Node<SCArgumentDeclare_I(maybe<SCAnyExpr_I>)> {
    static constexpr auto type_name { "SCArgumentDeclare" };
    SymSCIdentifier name; ///< The argument's name.
    LocalIndex local; ///< The argument.
    OptionalConstExprIndex prototype; ///< The constant it defaults to, if its default is one.
    SCArgumentDeclare(SymSCIdentifier n, LocalIndex l, OptionalConstExprIndex p): name(n), local(l), prototype(p) {}
};


/// @brief The arguments a method or function declares.
struct SCArgumentDeclareList : priv::List<SCArgumentDeclareList_I, SCArgumentDeclare_I> {
    static constexpr auto type_name { "SCArgumentDeclareList" };
};

/// @brief A method: its arguments and body.
struct SCMethod : priv::Node<SCMethod_I(SCArgumentDeclareList_I positional_arguments,
                                        SCArgumentDeclareList_I variadic_arguments, SCBody_I body)> {
    static constexpr auto type_name { "SCMethod" };
    Locatable<SymSCSelector> selector; ///< The method's name.
    std::optional<Locatable<SymSCPrimitive>> primitive; ///< The primitive it calls, if any.
    bool poisoned { false }; ///< Whether there were errors in it, so it can't be used.
    /// In a class extension, replacing any method of the class with the same selector.
    bool extension { false };
    /// Can differ from its class's, when it is in an extension.
    ast::LanguageVersion version;
    /// What calling the method returns, when it is known and has no effects. Only set without arguments, as their
    /// values aren't known until the method is called. Callers can't rely on it unless they know which method is
    /// dispatched.
    OptionalConstExprIndex constant_result {};
    /// Set when it is poisoned: a string constant of the errors in it, which the backend gives to the error that
    /// calling the method throws.
    OptionalConstExprIndex poison_message {};
    SCMethod(Locatable<SymSCSelector> s, std::optional<Locatable<SymSCPrimitive>> p, ast::LanguageVersion v):
        selector(s),
        primitive(p),
        version(v) {}
};
/// @brief A function: its arguments and body.
struct SCFunction : priv::Node<SCFunction_I(SCArgumentDeclareList_I positional_arguments,
                                            SCArgumentDeclareList_I variadic_arguments, SCBody_I body)> {
    static constexpr auto type_name { "SCFunction" };
    /// What calling the function returns, when it is known and has no effects, e.g., `{ 1 }`. The function itself isn't
    /// constant. Only set without arguments, as their values aren't known until the function is called.
    OptionalConstExprIndex constant_result {};
    OptionalSymSCSymbol name {}; ///< The name of the function, if it has one.
    bool closed { false }; ///< Whether it was written as a closed function, so can't use its surroundings.
};
/// @brief One item of a region, e.g., `a = 1` or `( var b = 2; b )`, compiled into its own function, so each can be
/// evaluated on its own.
struct SCRegion : priv::Node<SCRegion_I(SCFunction_I function)> {
    static constexpr auto type_name { "SCRegion" };
};
/// @brief The items of a region, e.g., `a = 1; ( var b = 2; b )`, evaluated in turn.
struct SCRegionList : priv::List<SCRegionList_I, SCRegion_I> {
    static constexpr auto type_name { "SCRegionList" };
    /// What evaluating the regions returns, the last one's, when it is known.
    OptionalConstExprIndex constant_result {};
};
/// @brief Code that isn't in the class library, e.g., a line evaluated in the IDE or an scd file, compiled as if it
/// were written inside `Interpreter:functionCompileContext`, so it can use the Interpreter's members. A file as a
/// function, e.g., `|x| x + 1`, is a function, otherwise it is a list of regions.
struct SCInterpreted : priv::Node<SCInterpreted_I(join<SCFunction_I, SCRegionList_I> code)> {
    static constexpr auto type_name { "SCInterpreted" };
    bool poisoned { false }; ///< Whether there were errors in it, so it can't be used.
    /// Given when it is compiled, see IRCompiler::compile_interpreted.
    ast::LanguageVersion version;
    SCInterpreted(ast::LanguageVersion v): version(v) {}
};

/// @brief The methods of a class.
struct SCMethodList : priv::List<SCMethodList_I, SCMethod_I> {
    static constexpr auto type_name { "SCMethodList" };
};

/// @brief A declared instance variable, class variable, or constant. `accessor` is the `<`, `>`, or `<>` written.
struct SCMember : priv::Terminal<SCMember_I> {
    static constexpr auto type_name { "SCMember" };

    /// The node's location is the member's name.
    SCMember(SymSCIdentifier name, SymSCClassName owning_class): name(name), owning_class(owning_class) {}

    SymSCIdentifier name; ///< The member's name.
    SymSCClassName owning_class; ///< The class that declares it.
    OptionalConstExprIndex expr {}; ///< The constant it is initialised to, if it is one.
    TypeInfo expr_type_info {}; ///< What is known about the class of the initial value.

    ast::ReadWriteAccessor accessor {}; ///< The accessors it declares.
    bool is_const {}; ///< Whether it is a constant.
    /// Can be replaced by extentions.
    OptionalSymSCSelector getter {};
    OptionalSymSCSelector setter {};
};

/// @brief The members of a class.
struct SCMemberList : priv::List<SCMemberList_I, SCMember_I> {
    static constexpr auto type_name { "SCMemberList" };
};

/// @brief A class: its members and methods.
struct SCClass : priv::Node<SCClass_I(SCMemberList_I members, SCMethodList_I methods)> {
    static constexpr auto type_name { "SCClass" };
    SymSCClassName name; ///< The class's name.
    /// None for the root class. A meta class's is its superclass's meta class, or Class for the root's.
    std::optional<SymSCClassName> superclass {};
    /// How an instance's indexed slots, its elements, are stored, set by the class's slot type, e.g., `String[char]`.
    /// A class without one, NotIndexed, only has its instance variables.
    MemoryLayout memory_layout { MemoryLayout::NotIndexed };
    std::optional<SymSCClassName> meta_class {}; ///< The class's meta class, none for a meta class.
    /// Of its own declaration, its extension methods have their own, see SCMethod::version.
    ast::LanguageVersion version { ast::LanguageVersion::Legacy };
    SCClass(SymSCClassName n): name(n) {}
};


/// @brief A sequence of expressions, the last is the value.
struct SCExprList : priv::List<TypedNodeIndex<SCNodeType::ExprList>, SCAnyExpr_I> {
    static constexpr auto type_name { "SCExprList" };
};


}
