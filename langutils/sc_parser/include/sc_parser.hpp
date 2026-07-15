// Copyright Jordan Henderson 2026
#pragma once
#include "node_graph.hpp"
#include "text_location.hpp"
#include "text_info.hpp"
#include <tuple>
#include <memory>

namespace sc::parser {
[[nodiscard]] std::tuple<graph::NodeGraph, bool> parse(std::shared_ptr<const TextInfo>, sc::lex::SourceCodeRange);
[[nodiscard]] std::tuple<graph::NodeGraph, bool> parse(std::shared_ptr<const TextInfo>, sc::lex::SourceCodeLocation);
[[nodiscard]] std::tuple<graph::NodeGraph, bool> parse(std::shared_ptr<const TextInfo>);
}
