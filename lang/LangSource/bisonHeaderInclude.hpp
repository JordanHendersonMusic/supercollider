#pragma once

#include "lexer.hpp"

struct EmptyYYVal {};

using Location = sc::lex::SourceCodeRange;

enum struct YYSTypeTag {
    node,
    slotNode,
    varListNode,
    varDefNode,
    methodNode,
    argListNode,
    multiAssignListNode,
    rwAccessor,
    empty
};

// This macro defines the default rule for how to combine location types. It is used in the parser.
#define YYLLOC_DEFAULT(Current, Rhs, N)                                                                                \
    do                                                                                                                 \
        if ((N) == 0) {                                                                                                \
            (Current) = YYRHSLOC(Rhs, 0);                                                                              \
        } else {                                                                                                       \
            (Current) = YYRHSLOC(Rhs, 1).span_to(YYRHSLOC(Rhs, N));                                                    \
        }                                                                                                              \
    while (0)
