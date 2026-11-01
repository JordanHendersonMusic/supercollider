#pragma once

#include "sc_query/text.hpp"

#include "sc_sema/ir.hpp"

#include <string_view>

namespace sc::query {

/// @brief The location of a name at the start of a node's location, e.g., a class's name in its declaration.
[[nodiscard]] Location name_location(const ir::IRGraph& g, const ir::SourceLocation& l, std::string_view name);

}
