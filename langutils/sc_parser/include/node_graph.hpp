// Copyright Jordan Henderson 2026
#pragma once

#include "index.hpp"
#include "text_location.hpp"
#include "nodes.hpp"
#include <type_traits>
#include <vector>

namespace sc::parser::nodes {

struct Edges {
    MaybeIndex parent {};
    MaybeIndex next_sibling {};
    // Last sibling is either 'this', if a single node; the last sibling, if 'this' is the first node; else nil.
    MaybeIndex last_sibling {};
    MaybeIndex first_child {};
};

}

namespace sc::parser::graph {

namespace priv {

}

class ParserGraph {
public:
    ParserGraph() = default;
    ParserGraph(ParserGraph&&) noexcept = default;
    //
    ParserGraph(const ParserGraph&) = delete;
    ParserGraph& operator=(ParserGraph&&) noexcept = delete;
    ParserGraph& operator=(const ParserGraph&) = delete;


    template <typename Node, class... ARGS>
    [[nodiscard]] auto create(Node n, sc::lex::SourceCodeRange loc, ARGS... args) {
        return std::move(n).create(*this, loc, std::forward<ARGS>(args)...);
    }

    template <NodeFlag... Ps, NodeFlag... Cs>
    TypedIndex<Ps...> append_to_list(TypedIndex<Ps...> p, TypedIndex<Cs...> c) {
        using ParentIndex = TypedIndex<Ps...>;
        using ChildIndex = TypedIndex<Cs...>;
        using ParentType = typename decltype(nodes::NodeCollection::get_node_type_from_index_type<ParentIndex>())::type;
        static_assert(std::is_constructible_v<typename ParentType::ChildIndex, ChildIndex>,
                      "Attempting to add the wrong child type.");

        append_child_init(*p, *c);
        return p;
    }

    template <NodeFlag... Ts> [[nodiscard]] auto& get_payload(TypedIndex<Ts...> index) {
        using PayloadT =
            typename decltype(nodes::NodeCollection::get_node_type_from_index_type<TypedIndex<Ts...>>())::type;
        return std::get<PayloadT>(payloads[*index]);
    }

    template <NodeFlag... Ts> [[nodiscard]] const auto& get_payload(TypedIndex<Ts...> index) const {
        using PayloadT =
            typename decltype(nodes::NodeCollection::get_node_type_from_index_type<TypedIndex<Ts...>>())::type;
        return std::get<PayloadT>(payloads[*index]);
    }

    [[nodiscard]] sc::lex::SourceCodeRange& get_location(Index index) { return locations[*index]; }
    [[nodiscard]] const sc::lex::SourceCodeRange& get_location(Index index) const { return locations[*index]; }

    struct Ref {
        sc::lex::SourceCodeRange& range;
        nodes::Edges& edges;
        nodes::NodeVariant& payload;
    };
    struct ConstRef {
        const sc::lex::SourceCodeRange& range;
        const nodes::Edges& edges;
        const nodes::NodeVariant& payload;
    };

    Ref ref(Index i) &;
    ConstRef ref(Index i) const&;
    Ref ref(Index i) && = delete;

    template <typename F> void flat_walk(F&& f) const {
        const auto sz = edges.size();
        for (size_t i { 0 }; i < sz; ++i) {
            f(locations[i], edges[i], payloads[i], i);
        }
    }
    template <typename F> void flat_walk(F&& f) {
        const auto sz = edges.size();
        for (size_t i { 0 }; i < sz; ++i) {
            f(locations[i], edges[i], payloads[i], i);
        }
    }

    // will go crazy if the graph contains a cycle.
    template <typename F> void depth_first_traverse(F&& f) {
        if (edges.empty())
            return;
        depth_first_traverse_impl(*this, f, edges.size() - 1, 0);
    }
    template <typename F> void depth_first_traverse(F&& f) const {
        if (edges.empty())
            return;
        depth_first_traverse_impl(*this, f, edges.size() - 1, 0);
    }

    template <typename F> void children_traverse(F&& f, Index i) {
        const auto& h = edges[*i];
        if (h.first_child) {
            for (MaybeIndex c = h.first_child; c; c = edges[*c].next_sibling) {
                f(locations[*c], edges[*c], payloads[*c], Index { *c });
            }
        }
    }
    template <typename F> void children_traverse(F&& f, Index i) const {
        const auto& h = edges[*i];
        if (h.first_child) {
            for (MaybeIndex c = h.first_child; c; c = edges[*c].next_sibling) {
                f(locations[*c], edges[*c], payloads[*c], Index { *c });
            }
        }
    }

    template <class I> friend struct nodes::priv::NodeBase;

private:
    std::vector<sc::lex::SourceCodeRange> locations {};
    std::vector<nodes::Edges> edges {};
    std::vector<nodes::NodeVariant> payloads {};

    // This is untyped. Instead use the IRNode's create method (it's a friend class).
    Index create_impl(sc::lex::SourceCodeRange range, nodes::Edges h, nodes::NodeVariant var);
    void append_child_init(parser::Index parent, parser::Index child);


    template <typename SELF, typename F>
    static void depth_first_traverse_impl(SELF& self, F& f, size_t i, size_t depth = 0, size_t visited = 0) {
        assert(visited < 999'999'999); // just a silly number to make sure we don't get stuck in a loop

        const auto& h = self.edges[i];
        f(self.locations[i], h, self.payloads[i], i, depth, visited + 1);

        if (h.first_child)
            depth_first_traverse_impl(self, f, *h.first_child, depth + 1, visited + 1);

        // last sibling is only valid for the first node.

        if (h.next_sibling)
            depth_first_traverse_impl(self, f, *h.next_sibling, depth, visited + 1);
    }

    template <typename SELF, typename F>
    static void breath_first_traverse_impl(SELF& self, F& f, size_t i, size_t depth = 0, size_t visited = 0) {
        assert(visited < 999'999'999); // just a silly number to make sure we don't get stuck in a loop

        const auto& h = self.edges[i];
        f(self.locations[i], h, self.payloads[i], i, depth);

        if (h.last_sibling) {
            for (auto c = h.next_sibling; c; c = self.edges[*c].next_sibling) {
                breath_first_traverse_impl(f, *c, depth, visited + 1);
            }
        }

        if (h.first_child)
            depth_first_traverse_impl(self, f, *h.first_child, depth + 1, visited + 1);
    }
};

}


namespace sc::parser::nodes::priv {
template <class IndexT>
template <class T, class ThisTypedIndex, typename... CHILD_INDEXES>
inline ThisTypedIndex NodeBase<IndexT>::create(graph::ParserGraph& c, sc::lex::SourceCodeRange range,
                                               CHILD_INDEXES&&... children) && {
    const auto i = c.create_impl(range, {}, NodeVariant { static_cast<T&&>(*this) });

    (c.append_child_init(i, *children), ...);

    return ThisTypedIndex { *i };
};


template <typename T, class ThisTypedIndex, typename ChildIndexT>
template <class... CS>
inline ThisTypedIndex ListNode<T, ThisTypedIndex, ChildIndexT>::create(graph::ParserGraph& g,
                                                                       sc::lex::SourceCodeRange range, CS&&... cs) && {
    const ThisTypedIndex r =
        static_cast<NodeBase<ThisTypedIndex>&&>(*this).template create<T, ThisTypedIndex>(g, range);


    (g.append_to_list(r, std::forward<CS>(cs)), ...);

    return r;
}
}
