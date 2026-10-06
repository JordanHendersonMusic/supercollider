// Copyright Jordan Henderson 2026
#include "node_graph.hpp"
#include "index.hpp"
#include "indexes_typed.hpp"

namespace sc::parser::graph {

[[nodiscard]] std::optional<Index> NodeGraph::root_any() const {
    using T = std::optional<Index>;
    return std::visit(priv::overload { [](std::monostate) -> T { return std::nullopt; },
                                       [](auto i) -> T { return { Index { i.value() } }; } },
                      m_roots);
}
[[nodiscard]] std::variant<std::monostate, RegionListIndex, ClassOrExtensionListIndex> NodeGraph::root() const {
    return m_roots;
}

[[nodiscard]] NodeGraph::operator bool() const { return !m_has_fatal_diagnostic; }
void NodeGraph::add_diagnostic(Diagnostic d) {
    m_has_fatal_diagnostic |= d.fatal();
    m_diagnostics.push_back(std::move(d));
}


[[nodiscard]] const sc::lex::SourceCodeRange& NodeGraph::location(Index index) const { return m_locations[*index]; }
[[nodiscard]] sc::lex::SourceCodeRange& NodeGraph::location(Index index) { return m_locations[*index]; }

[[nodiscard]] nodes::Edges& NodeGraph::edges(Index i) noexcept {
    assert(*i < m_edges.size());
    return m_edges[*i];
}
[[nodiscard]] const nodes::Edges& NodeGraph::edges(Index i) const noexcept {
    assert(*i < m_edges.size());
    return m_edges[*i];
}


std::vector<Index> NodeGraph::orphans() const {
    std::vector<Index> out;
    out.reserve(4); // there is usually only 1.
    const auto sz = m_edges.size();
    for (Index::Underlying i { 0 }; i < sz; ++i) {
        if (!m_edges[i].parent
            && std::find(m_dead_nodes.begin(), m_dead_nodes.end(), i)
                == m_dead_nodes.end()) // if there is no parent, and the node isn't dead, push it
            out.push_back(Index { i });
    }
    return out;
}

RegionListIndex NodeGraph::assign_root(RegionListIndex i) {
    assert(i);
    disconnect_sub_graph({ *i });
    m_roots = i;
    return i;
}

ClassOrExtensionListIndex NodeGraph::assign_root(ClassOrExtensionListIndex i) {
    assert(i);
    disconnect_sub_graph({ *i });
    m_roots = i;
    return i;
}


void NodeGraph::disconnect_sub_graph(Index i) {
    auto& e = m_edges[*i];

    // disconnect parent and sibling connections to this.
    [&]() {
        if (e.parent) {
            auto& p = m_edges[*e.parent];

            if (*p.first_child == *i) {
                p.first_child = e.next_sibling;
                if (e.next_sibling) {
                    m_edges[*e.next_sibling].last_sibling = e.last_sibling;
                }
                e.parent = {};
                return;
            }

            m_edges[*e.last_sibling].next_sibling = e.next_sibling;
            if (e.next_sibling) {
                m_edges[*e.next_sibling].previous_sibling = e.last_sibling;
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
    auto& parent_edges = m_edges[*parent];

    // assign all children's parent index
    Index last_new = *child;
    for (OptionalIndex c = child; c; c = m_edges[*c].next_sibling) {
        m_edges[*c].parent = parent;
        last_new = *c;
        m_edges[*c].last_sibling = OptionalIndex {}; // remove the last sibling from all of them
    }

    if (parent_edges.first_child) {
        // Already has a child. Append.
        auto& first = m_edges[*parent_edges.first_child];
        if (first.last_sibling) {
            auto& last = m_edges[*first.last_sibling];
            m_edges[*child].previous_sibling = first.last_sibling;
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
        m_edges[*child].last_sibling = last_new;
    }
}

} // sc::parser::graph
