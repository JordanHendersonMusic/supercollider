// Copyright Jordan Henderson 2026
#pragma once
#include "sc_source/text_location.hpp"
#include "sc_source/text_info.hpp"
#include "sc_lexer/details/tokens.hpp"
#include <tuple>
#include <memory>
#include <optional>
#include <vector>

namespace sc::ast {

struct ASTGraph;

/// @name Parsing
/// Each overload returns the graph, and whether the parser accepted the input. Errors are recovered from, so input
/// with errors is usually accepted, check the graph's diagnostics (ASTGraph::diagnostics) for them.
/// @{

/// @brief Parses only the given range of the text, e.g., one snippet of a larger file.
/// @param text The text, kept alive by the returned graph.
/// @param range The part of @p text to parse.
/// @return The graph, and whether the input was accepted.
[[nodiscard]] std::tuple<std::shared_ptr<ASTGraph>, bool> parse(std::shared_ptr<const sc::src::TextInfo> text,
                                                                sc::src::SourceCodeRange range);
/// @brief Parses from a location until the end of the text.
/// @param text The text, kept alive by the returned graph.
/// @param start Where to begin parsing.
/// @return The graph, and whether the input was accepted.
[[nodiscard]] std::tuple<std::shared_ptr<ASTGraph>, bool> parse(std::shared_ptr<const sc::src::TextInfo> text,
                                                                sc::src::SourceCodeLocation start);
/// @brief Parses the whole text.
/// @param text The text, kept alive by the returned graph.
/// @return The graph, and whether the input was accepted.
[[nodiscard]] std::tuple<std::shared_ptr<ASTGraph>, bool> parse(std::shared_ptr<const sc::src::TextInfo> text);
/// @}

/// @brief The tokens the parser can take at the end of @p range, e.g., `KeywordBinaryOperator`, but not `Name`, after
/// `Foo` in `Foo ba`, as two expressions can't be next to each other.
///
/// Parses @p range as parse() does, recovering from errors, and stops at its end, before the brackets left open are
/// closed. In an scd file, the tokens a missing semicolon at a new line would allow are included.
/// @param text The text.
/// @param range The part of @p text before the place asked about, usually from its start.
/// @return The token types, in no particular order. Empty if the parse ended before the end of @p range. Nothing if
/// error recovery skipped up to the end, e.g., in `(x = ; y.`, where the error drops the whole bracket, so what the
/// parser expects is only what follows the error.
[[nodiscard]] std::optional<std::vector<sc::lex::TokenType>>
expected_tokens(std::shared_ptr<const sc::src::TextInfo> text, sc::src::SourceCodeRange range);
}
