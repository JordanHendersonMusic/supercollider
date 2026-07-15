#include "node_graph.hpp"
#include "index.hpp"

namespace sc::parser::graph {

ParserGraph::Ref ParserGraph::ref(Index i) & { return { locations[*i], edges[*i], payloads[*i] }; }

ParserGraph::ConstRef ParserGraph::ref(Index i) const& { return { locations[*i], edges[*i], payloads[*i] }; }

Index ParserGraph::create_impl(sc::lex::SourceCodeRange range, nodes::Edges h, nodes::NodeVariant var) {
    assert(locations.size() == edges.size());
    assert(locations.size() == payloads.size());
    const auto i = edges.size();
    locations.push_back(range);
    edges.push_back(h);
    payloads.push_back(var);
    return i;
}

void ParserGraph::append_child_init(parser::Index parent, parser::Index child) {
    auto& parent_edges = edges[*parent];

    // assign all children's parent index
    Index last_new = child;
    for (MaybeIndex c = child; c; c = edges[*c].next_sibling) {
        edges[*c].parent = parent;
        last_new = *c;
        edges[*c].last_sibling = MaybeIndex {}; // remove the last sibling from all of them
    }

    if (parent_edges.first_child) {
        // Already has a child. Append.
        auto& first = edges[*parent_edges.first_child];
        if (first.last_sibling) {
            auto& last = edges[*first.last_sibling];
            last.next_sibling = child;
            first.last_sibling = last_new;
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
