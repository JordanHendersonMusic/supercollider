#pragma once
#include "node_graph.hpp"
#include "parser_error_handler.hpp"
#include "text_info.hpp"
#include "text_location.hpp"
#include <memory>
#include <tuple>

namespace sc::parser {

[[nodiscard]] std::tuple<graph::ParserGraph, bool> parse(std::shared_ptr<const TextInfo>, sc::lex::SourceCodeRange,
                                                         std::shared_ptr<ErrorHandler> err);
[[nodiscard]] std::tuple<graph::ParserGraph, bool> parse(std::shared_ptr<const TextInfo>, sc::lex::SourceCodeLocation,
                                                         std::shared_ptr<ErrorHandler> err);
[[nodiscard]] std::tuple<graph::ParserGraph, bool> parse(std::shared_ptr<const TextInfo>,
                                                         std::shared_ptr<ErrorHandler> err);

}
