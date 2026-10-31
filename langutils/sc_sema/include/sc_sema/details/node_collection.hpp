#pragma once

#include "sc_sema/details/indexes.hpp"
#include "sc_sema/details/ir_nodes.hpp"
#include "sc_util/details/typed_graph_helper.hpp"

namespace sc::ir {

/// @brief Every kind of node in an IRGraph, in the order of SCNodeType.
using NodeCollectionHelper = sc::util::typed_graph::TypedGraphHelper<SCNodeType, TypedNodeIndex, //
                                                                     SCMissing, //
                                                                     SCLiteral, //
                                                                     SCKwArg, //
                                                                     SCPositionalArgs, //
                                                                     SCKwArgs, //
                                                                     SCVariadicArgs, //
                                                                     SCAssign, //
                                                                     SCRead, //
                                                                     SCSpecial, //
                                                                     SCMessage, //
                                                                     SCVariableDeclare, //
                                                                     SCExplicitReturn, //
                                                                     SCCollection, //
                                                                     SCRange, //
                                                                     SCPackTarget, //
                                                                     SCPackTargets, //
                                                                     SCPackAssign, //
                                                                     SCPackDeclare, //
                                                                     SCGenQualIterate, //
                                                                     SCGenQualDeclare, //
                                                                     SCGenQualIf, //
                                                                     SCGenQualSideEffect, //
                                                                     SCGenQualWhile, //
                                                                     SCGenQualList, //
                                                                     SCRoutineGenerator, //
                                                                     SCImmediateGenerator, //
                                                                     SCRangeGenerator, //
                                                                     SCBody, //
                                                                     SCArgumentDeclare, //
                                                                     SCArgumentDeclareList, //
                                                                     SCMethod, //
                                                                     SCFunction,
                                                                     SCInterpreted, //
                                                                     SCRegion, //
                                                                     SCRegionList, //
                                                                     SCMethodList, //
                                                                     SCMember, //
                                                                     SCMemberList, //
                                                                     SCClass, //
                                                                     SCExprList //
                                                                     >;

}
