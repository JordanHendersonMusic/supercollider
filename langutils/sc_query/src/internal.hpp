#pragma once

#include "sc_query/document.hpp"
#include "sc_query/library.hpp"
#include "sc_query/queries.hpp"
#include "sc_query/text.hpp"

#include "sc_sema/details/const_expr.hpp"
#include "sc_sema/ir.hpp"

#include <optional>
#include <string_view>
#include <utility>

namespace sc::query {

/// @brief The location of a name at the start of a node's location, e.g., a class's name in its declaration.
[[nodiscard]] Location name_location(const ir::IRGraph& g, const ir::SourceLocation& l, std::string_view name);

/// @brief The class of a constant, and whether it is the class itself, e.g., `Array` for `#[1]`.
[[nodiscard]] std::pair<std::optional<ClassId>, bool> constant_class(const Library& library,
                                                                     const ir::ConstExpression& c);
/// @brief The class of an expression when it is known, and whether it is the class itself: a literal, a collection, a
/// function, or `Foo.new`, which is guessed to make a Foo.
[[nodiscard]] std::pair<std::optional<ClassId>, bool> expression_class(const Library& library, const ir::IRGraph& g,
                                                                       ir::NodeIndex e);
/// @brief Compiles @p code as interpreted code, and gives the class of what it evaluates to when known, see
/// expression_class(). Nothing when it has errors.
[[nodiscard]] std::pair<std::optional<ClassId>, bool> compiled_class(Library& library, std::string_view code);

/// @brief The names in a compile of interpreted code, see analyse_names(const Document&).
[[nodiscard]] Analysis analyse_names(const CompiledText& compiled);
}
