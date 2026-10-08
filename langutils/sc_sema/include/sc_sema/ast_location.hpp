#pragma once
#include "sc_parser/index.hpp"
#include "sc_util/strong_index.hpp"
#include <cstdint>

namespace sc::ir {

using ASTGraphIndex = util::StrongIndex<std::uint32_t, struct ASTGraphIndex__>;

struct ASTLocation {
    ASTGraphIndex graph_index;
    sc::ast::Index node_index;
};

}
