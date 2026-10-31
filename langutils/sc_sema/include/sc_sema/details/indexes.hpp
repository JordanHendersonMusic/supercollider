#pragma once
#include "sc_util/strong_index.hpp"
#include "sc_util/type_set_index.hpp"
#include <cstdint>
#include <limits>
namespace sc::ir {
/// @brief The type of every node in an IRGraph. Typed indexes are sets of these.
enum struct SCNodeType {
    Missing,
    Message,

    Assign,
    Read,
    Special,


    KwArg,
    VariableDeclare,
    ExplicitReturn,

    KwArgList,
    PosArgList,
    VariadicArgList,
    Literal,
    Collection, ///< A collection that can't be turned into a literal.
    Range,
    PackAssign,
    PackDeclare,
    PackTarget,
    PackTargetList,
    RoutineGenerator,
    ImmediateGenerator,
    GenQualIterate,
    GenQualDeclare,
    GenQualIf,
    GenQualSideEffect,
    GenQualWhile,
    GenQualList,
    RangeGenerator,

    ArgumentDeclare,
    ArgumentDeclareList,
    Method,
    Function,
    Interpreted,
    Region,
    RegionList,
    MethodList,
    Member,
    MemberList,
    Class,
    Extention,

    ExprList,
    Body,
    ClassOrExtentionList,
};

////////////////////////////////////////////////////////////////////////////////

/// @brief The Spec of the IR node indexes.
using NodeSpec = sc::util::typed_index::Spec<std::uint32_t, SCNodeType, struct Graph__>;
using NodeDef = util::typed_index::QuicklyDefineTypesFromSpec<NodeSpec>;
/// An index of a node of the IR graph that may be empty.
using OptNodeIndex = NodeDef::OptionalIndex;
/// An index of any node of the IR graph.
using NodeIndex = NodeDef::Index;

/// @brief An index that can only refer to nodes of the given types.
template <SCNodeType... ts> using TypedNodeIndex = NodeDef::TypedIndex<ts...>;

/// @brief The index that may refer to any node type that one of @p Ts may refer to.
template <typename... Ts> using join = sc::util::typed_index::join<Ts...>;
/// @brief @p T, or an SCMissing node when the thing is optional.
template <typename T> using maybe = join<T, TypedNodeIndex<SCNodeType::Missing>>;

////////////////////////////////////////////////////////////////////////////////

/// @name Typed indexes
/// One for each node type, named after the node, and the sets of them that are used together.
/// @{
using SCMissing_I = TypedNodeIndex<SCNodeType::Missing>;
using SCLiteral_I = TypedNodeIndex<SCNodeType::Literal>;


using SCMessage_I = TypedNodeIndex<SCNodeType::Message>;
using SCAssign_I = TypedNodeIndex<SCNodeType::Assign>;
using SCRead_I = TypedNodeIndex<SCNodeType::Read>;
using SCSpecial_I = TypedNodeIndex<SCNodeType::Special>;

using SCKwArgList_I = TypedNodeIndex<SCNodeType::KwArgList>;
using SCPosArgList_I = TypedNodeIndex<SCNodeType::PosArgList>;
using SCVariadicArgList_I = TypedNodeIndex<SCNodeType::VariadicArgList>;

using SCKwArg_I = TypedNodeIndex<SCNodeType::KwArg>;
using SCExplicitReturn_I = TypedNodeIndex<SCNodeType::ExplicitReturn>;

using SCVariableDeclare_I = TypedNodeIndex<SCNodeType::VariableDeclare>;

using SCCollection_I = TypedNodeIndex<SCNodeType::Collection>;
using SCRange_I = TypedNodeIndex<SCNodeType::Range>;
using SCPackAssign_I = TypedNodeIndex<SCNodeType::PackAssign>;
using SCPackDeclare_I = TypedNodeIndex<SCNodeType::PackDeclare>;
using SCPackTarget_I = TypedNodeIndex<SCNodeType::PackTarget>;
using SCPackTargetList_I = TypedNodeIndex<SCNodeType::PackTargetList>;
using SCRoutineGenerator_I = TypedNodeIndex<SCNodeType::RoutineGenerator>;
using SCImmediateGenerator_I = TypedNodeIndex<SCNodeType::ImmediateGenerator>;
using SCAnyGenerator_I = join<SCRoutineGenerator_I, SCImmediateGenerator_I>;
using SCGenQualIterate_I = TypedNodeIndex<SCNodeType::GenQualIterate>;
using SCGenQualDeclare_I = TypedNodeIndex<SCNodeType::GenQualDeclare>;
using SCGenQualIf_I = TypedNodeIndex<SCNodeType::GenQualIf>;
using SCGenQualSideEffect_I = TypedNodeIndex<SCNodeType::GenQualSideEffect>;
using SCGenQualWhile_I = TypedNodeIndex<SCNodeType::GenQualWhile>;
using SCGenQualList_I = TypedNodeIndex<SCNodeType::GenQualList>;
using SCRangeGenerator_I = TypedNodeIndex<SCNodeType::RangeGenerator>;
using SCArgumentDeclare_I = TypedNodeIndex<SCNodeType::ArgumentDeclare>;
using SCArgumentDeclareList_I = TypedNodeIndex<SCNodeType::ArgumentDeclareList>;
using SCMethod_I = TypedNodeIndex<SCNodeType::Method>;
using SCFunction_I = TypedNodeIndex<SCNodeType::Function>;
using SCInterpreted_I = TypedNodeIndex<SCNodeType::Interpreted>;
using SCRegion_I = TypedNodeIndex<SCNodeType::Region>;
using SCRegionList_I = TypedNodeIndex<SCNodeType::RegionList>;
using SCMethodList_I = TypedNodeIndex<SCNodeType::MethodList>;
using SCMember_I = TypedNodeIndex<SCNodeType::Member>;
using SCMemberList_I = TypedNodeIndex<SCNodeType::MemberList>;
using SCClass_I = TypedNodeIndex<SCNodeType::Class>;
using SCExtention_I = TypedNodeIndex<SCNodeType::Extention>;

using SCExprList_I = TypedNodeIndex<SCNodeType::ExprList>;
using SCBody_I = TypedNodeIndex<SCNodeType::Body>;

/// Anything that evaluates to a value.
using SCAnyExpr_I =
    join<SCExprList_I, SCFunction_I, SCAssign_I, SCRead_I, SCSpecial_I, SCExplicitReturn_I, SCLiteral_I, SCMessage_I,
         SCCollection_I, SCRange_I, SCPackAssign_I, SCRoutineGenerator_I, SCImmediateGenerator_I, SCRangeGenerator_I>;

using SCClassList_I = TypedNodeIndex<SCNodeType::ClassOrExtentionList>;

/// @brief Nodes that own local variables.
using SCLocalOwner_I = join<SCFunction_I, SCMethod_I, SCGenQualList_I>;

/// @}

/// @brief A local variable, an index into its IRGraph's table of them, see LocalVariable.
using LocalIndex = util::StrongIndex<std::uint32_t, struct LocalIndex__>;
/// A LocalIndex that may be empty.
using OptionalLocalIndex =
    util::StrongOptionalIndex<std::uint32_t, struct LocalIndex__, std::numeric_limits<std::uint32_t>::max()>;

/// @brief A member variable, an index into its IRGraph's table of them, see MemberVariable.
using MemberIndex = util::StrongIndex<std::uint32_t, struct MemberIndex__>;
/// A MemberIndex that may be empty.
using OptionalMemberIndex =
    util::StrongOptionalIndex<std::uint32_t, struct MemberIndex__, std::numeric_limits<std::uint32_t>::max()>;

/// @brief A constant in a ConstantTable.
using ConstExprIndex = util::StrongIndex<std::uint32_t, struct ConstantExpressionIndex__>;
/// A ConstExprIndex that may be empty.
using OptionalConstExprIndex = util::StrongOptionalIndex<std::uint32_t, struct ConstantExpressionIndex__,
                                                         std::numeric_limits<std::uint32_t>::max()>;

/// @brief An element of a constant array.
/// The elements are constants in the same ConstantTable, so nesting costs an index per element rather than a copy, and
/// as every constant is only in the table once, two arrays are equal exactly when their elements' indexes are.
using ConstArrayElementIndex = util::StrongIndex<std::uint32_t, struct ConstArrayElementIndex__>;

/// @brief An index into ClassLibrarySources, one per class file.
using SourceEntryIndex = util::StrongIndex<std::uint32_t, struct SourceEntryIndex__>;

/// @brief An index into an IRGraph's table of sources.
using SourceIndex = util::StrongIndex<std::uint32_t, struct SourceIndex__>;

/// @brief A path given to the compiler, see ModuleTable.
using ModuleIndex = util::StrongIndex<std::uint32_t, struct ModulePath__>;
}
