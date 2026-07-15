// Copyright Jordan Henderson 2026
#include "node_graph.hpp"
#include "index.hpp"
#include "indexes_typed.hpp"

namespace sc::parser::graph {

[[nodiscard]] std::optional<Index> NodeGraph::root_any() const {
    using T = std::optional<Index>;
    return std::visit(priv::overload { [](std::monostate) -> T { return std::nullopt; },
                                       [](auto i) -> T { return { Index { i.value() } }; } },
                      roots);
}
[[nodiscard]] std::variant<std::monostate, RegionListIndex, ClassOrExtensionListIndex> NodeGraph::root() const {
    return roots;
}

[[nodiscard]] NodeGraph::operator bool() const { return !has_fatal_diagnostic; }

[[nodiscard]] sc::lex::SourceCodeRange& NodeGraph::get_location(Index index) { return locations[*index]; }
[[nodiscard]] const sc::lex::SourceCodeRange& NodeGraph::get_location(Index index) const { return locations[*index]; }

std::vector<Index> NodeGraph::orphans() const {
    std::vector<Index> out;
    out.reserve(4); // there is usually only 1.
    const auto sz = edges.size();
    for (Index::IndexType i { 0 }; i < sz; ++i) {
        if (!edges[i].parent
            && std::find(dead_nodes.begin(), dead_nodes.end(), i)
                == dead_nodes.end()) // if there is no parent, and the node isn't dead, push it
            out.push_back(Index { i });
    }
    return out;
}

RegionListIndex NodeGraph::assign_root(RegionListIndex i) {
    assert(i);
    disconnect_sub_graph({ *i });
    roots = i;
    return i;
}

ClassOrExtensionListIndex NodeGraph::assign_root(ClassOrExtensionListIndex i) {
    assert(i);
    disconnect_sub_graph({ *i });
    roots = i;
    return i;
}


void NodeGraph::disconnect_sub_graph(Index i) {
    auto& e = edges[*i];

    // disconnect parent and sibling connections to this.
    [&]() {
        if (e.parent) {
            auto& p = edges[*e.parent];

            if (*p.first_child == *i) {
                p.first_child = e.next_sibling;
                if (e.next_sibling) {
                    edges[*e.next_sibling].last_sibling = e.last_sibling;
                }
                e.parent = {};
                return;
            }

            edges[*e.last_sibling].next_sibling = e.next_sibling;
            if (e.next_sibling) {
                edges[*e.next_sibling].previous_sibling = *e.last_sibling;
            }
            return;
        }
    }();

    // Remove our connections.
    e.parent = {};
    e.next_sibling = {};
    e.last_sibling = {};
    e.previous_sibling = {};
}
void NodeGraph::append_unchecked(parser::OptionalIndex parent, parser::OptionalIndex child) {
    if (!child || !parent)
        return;
    auto& parent_edges = edges[*parent];

    // assign all children's parent index
    Index last_new = *child;
    for (OptionalIndex c = child; c; c = edges[*c].next_sibling) {
        edges[*c].parent = parent;
        last_new = *c;
        edges[*c].last_sibling = OptionalIndex {}; // remove the last sibling from all of them
    }

    if (parent_edges.first_child) {
        // Already has a child. Append.
        auto& first = edges[*parent_edges.first_child];
        if (first.last_sibling) {
            auto& last = edges[*first.last_sibling];
            edges[*child].previous_sibling = first.last_sibling;
            last.next_sibling = child;
            first.last_sibling = sc::util::typed_index::as_optional(last_new);
        } else {
            first.next_sibling = child;
            first.last_sibling = last_new;
            // only one child
        }
    } else {
        // This is the first child.
        parent_edges.first_child = child;
        edges[*child].last_sibling = last_new;
    }
}

} // sc::parser::graph
