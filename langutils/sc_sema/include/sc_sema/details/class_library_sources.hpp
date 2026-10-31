#pragma once

#include "sc_parser/details/indexes_typed.hpp"
#include "sc_sema/details/indexes.hpp"
#include "sc_util/indexed_vector.hpp"

#include <memory>
#include <optional>

namespace sc::ast {
struct ASTGraph;
}

namespace sc::ir {

/// @brief A parsed file of the class library.
struct SourceEntry {
    std::shared_ptr<const ast::ASTGraph> graph; ///< The parsed file.
    ast::ClassOrExtensionListIndex index; ///< The root of the file in the graph.
    ModuleIndex module; ///< The module the file is in.
};

/// @brief Each file of the class library, empty when it didn't parse.
using ClassLibrarySources = util::IndexedVector<SourceEntryIndex, std::optional<SourceEntry>>;

}
