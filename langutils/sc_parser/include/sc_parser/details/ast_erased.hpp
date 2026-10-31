// Copyright Jordan Henderson 2026
#pragma once

/// @file
/// @brief Free functions that mirror ASTGraph members, so headers can use the graph without including it.
///
/// Including the whole ast graph is slow (particularly for clangd), this provides some type erasure.

#include "indexes_typed.hpp"
#include "sc_source/text_location.hpp"
#include "sc_util/details/typed_graph_iter.hpp"
#include <memory>
#include <string>
#include <string_view>

namespace sc::src {
struct TextInfo;
}
namespace sc::ast {
struct ASTGraph;
struct IntNode;
struct FloatNode;
struct NonLocalReturnExpr;

/// @brief The text the graph was parsed from, see ASTGraph::text_info().
[[nodiscard]] const std::shared_ptr<const sc::src::TextInfo>& get_text_info(const ASTGraph&);

/// @brief Where in the source a node is, see ASTGraph::location().
[[nodiscard]] sc::src::SourceCodeRange get_location(const ASTGraph&, OptionalIndex);
/// @brief The source text of a node, see ASTGraph::string_view().
[[nodiscard]] std::string_view get_string_view(const ASTGraph&, OptionalIndex);
/// @brief The source text of a node as a copy, see ASTGraph::string().
[[nodiscard]] std::string get_string(const ASTGraph&, OptionalIndex);

/// @brief The node after @p i in its parent's list of children.
/// @return The sibling, or an empty index if @p i is the last child.
[[nodiscard]] OptionalIndex next_sibling(const ASTGraph&, OptionalIndex i);

/// @brief Narrows an expression index to the one of its possible typed indexes it holds.
[[nodiscard]] AnyExprIndex::Variant to_variant(const ASTGraph&, AnyExprIndex);
/// @brief Narrows a block item index to the one of its possible typed indexes it holds.
[[nodiscard]] BlockItemIndex::Variant to_variant(const ASTGraph&, BlockItemIndex);

/// @brief Deleted so other index types are not silently accepted.
/// If you have encountered this, add an overload above with the specific type.
template <typename T> AnyExprIndex::Variant to_variant(const ASTGraph&, T) = delete;

/// @brief The data stored in an integer literal node, see ASTGraph::payload().
[[nodiscard]] const IntNode& payload(const ASTGraph&, IntLitIndex);
/// @brief The data stored in a float literal node, see ASTGraph::payload().
[[nodiscard]] const FloatNode& payload(const ASTGraph&, FloatLitIndex);

/// @brief Iterates over the expressions in an expression sequence.
[[nodiscard]] util::typed_graph::Iter<OptionalIndex, AnyExprIndex> iter(const ASTGraph&, ExprSeqIndex);

/// @brief The expression a non local return (`^expr`) returns.
[[nodiscard]] AnyExprIndex child(const ASTGraph&, NonLocalReturnExpr);
}
