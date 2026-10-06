#pragma once
#include <cstdint>
namespace sc::ir {

struct ASTLocation {
    std::uint32_t container_index;
    std::uint32_t node_index;
};

}
