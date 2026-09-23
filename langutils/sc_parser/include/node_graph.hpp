// Copyright Jordan Henderson 2026
#pragma once

#include "index.hpp"
#include "indexes_typed.hpp"
#include "node_graph_diagnostic.hpp"
#include "sc_grammar_shared.hpp"
#include "text_location.hpp"
#include "nodes.hpp"
#include <algorithm>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace sc::parser::nodes {

struct Edges {
    OptionalIndex parent {};
    OptionalIndex next_sibling {};
    OptionalIndex previous_sibling {};
    OptionalIndex last_sibling {};
    OptionalIndex first_child {};

    void reset_all_but_first_child() {
        parent = {};
        next_sibling = {};
        previous_sibling = {};
        last_sibling = {};
    }
};

}

namespace sc::parser::graph {

namespace priv {
template <typename... Os> struct overload : Os... { using Os::operator()...; };

template <typename... Os> overload(Os...) -> overload<Os...>;
}

class NodeGraph {
public:
    NodeGraph() = default;
    NodeGraph(NodeGraph&&) noexcept = default;
    //
    NodeGraph(const NodeGraph&) = delete;
    NodeGraph& operator=(NodeGraph&&) noexcept = delete;
    NodeGraph& operator=(const NodeGraph&) = delete;

    [[nodiscard]] std::vector<Index> orphans() const;

    RegionListIndex assign_root(RegionListIndex i);
    ClassOrExtensionListIndex assign_root(ClassOrExtensionListIndex i);

    [[nodiscard]] std::optional<Index> root_any() const;
    [[nodiscard]] std::variant<std::monostate, RegionListIndex, ClassOrExtensionListIndex> root() const;

    // Hasn't encountered a fatal diagnostic (has no 'errors').
    [[nodiscard]] explicit operator bool() const;

    void add_diagnostic(Diagnostic d) {
        has_fatal_diagnostic |= d.fatal();
        m_diagnostics.push_back(std::move(d));
    }

    [[nodiscard]] const std::vector<Diagnostic>& diagnostics() const& { return m_diagnostics; }
    [[nodiscard]] std::vector<Diagnostic> diagnostics() && { return std::move(m_diagnostics); }

    template <typename Node, class... CHILDREN>
    [[nodiscard]] auto create(Node n, sc::lex::SourceCodeRange loc, CHILDREN... children);

    template <typename To, NodeFlag... Froms, typename... ARGS>
    auto cast(TypedIndex<Froms...> from_index, ARGS&&... args) -> typename To::IndexType;

    template <NodeFlag... Ps>
    TypedIndex<Ps...> append_to_list(TypedIndex<Ps...> p, sc::lex::SourceCodeRange) =
        delete; // !!!NOTE!!! If this has occurred, you have written '@n' instead of '$n' in the grammar file.

    // Updates the location data of the parent.
    template <NodeFlag... Ps, typename... Cs>
    TypedIndex<Ps...> append_to_list(TypedIndex<Ps...> p, sc::lex::SourceCodeRange range, Cs... cs);

    template <NodeFlag... Ps, NodeFlag... Cs>
    TypedIndex<Ps...> append_to_list(TypedIndex<Ps...> p, TypedIndex<Cs...> c);

    template <NodeFlag... Ps, NodeFlag... Cs>
    TypedIndex<Ps...> prepend_to_list(TypedIndex<Ps...> list, TypedIndex<Cs...> item);

    // After this, the child_list should be thought of as being in a moved from state, do not use it.
    template <NodeFlag... Ps, NodeFlag... Cs>
    TypedIndex<Ps...> merge_list(TypedIndex<Ps...> parent, TypedIndex<Cs...> child_list);

    // Used to replace one subgraph with the other.
    // This will replace the variant and the children (but no other edges).
    // It will keep the old location unless specified
    template <NodeFlag... OLDs, NodeFlag... NEWs>
    TypedIndex<OLDs...> subsituted(TypedIndex<OLDs...> old, TypedIndex<NEWs...> n);


    template <NodeFlag... Ts> [[nodiscard]] auto& get_payload(TypedIndex<Ts...> index);
    template <NodeFlag... Ts> [[nodiscard]] const auto& get_payload(TypedIndex<Ts...> index) const;
    [[nodiscard]] nodes::NodeVariant& get_payload(Index index) { return payloads[*index]; };
    [[nodiscard]] const nodes::NodeVariant& get_payload(Index index) const { return payloads[*index]; };

    template <typename TypedIndexT> [[nodiscard]] bool is_a(Index i) const noexcept;

    [[nodiscard]] sc::lex::SourceCodeRange& get_location(Index index);
    [[nodiscard]] const sc::lex::SourceCodeRange& get_location(Index index) const;

    [[nodiscard]] nodes::Edges get_edges(Index i) const noexcept {
        assert(*i < edges.size());
        return edges[*i];
    }


    // Mostly useful for debugging purposes.
    template <typename F> void flat_walk(F&& f) const;

    struct EmptyAction {
        constexpr void operator()() const {}
    };

    template <class EnterNode, class BeforeChildren = EmptyAction, class AfterChildren = EmptyAction,
              class ExitNode = EmptyAction>
    void depth_first_traverse(Index i, EnterNode&& enter_node, BeforeChildren&& before_children = {},
                              AfterChildren&& after_children = {}, ExitNode&& exit_node = {});

    template <class EnterNode, class BeforeChildren = EmptyAction, class AfterChildren = EmptyAction,
              class ExitNode = EmptyAction>
    void breadth_first_traverse(Index i, EnterNode&& enter_node, BeforeChildren&& before_children = {},
                                AfterChildren&& after_children = {}, ExitNode&& exit_node = {});

    template <typename F> void traverse_only_children(F&& f, Index i);

private:
    std::vector<sc::lex::SourceCodeRange> locations {};
    std::vector<nodes::Edges> edges {};
    std::vector<nodes::NodeVariant> payloads {};

    std::vector<Index::IndexType> dead_nodes {};

    std::vector<Diagnostic> m_diagnostics;
    bool has_fatal_diagnostic { false };

    std::variant<std::monostate, RegionListIndex, ClassOrExtensionListIndex> roots {};

    void disconnect_sub_graph(Index i);

    template <typename T>
    [[nodiscard]] Index create_impl(sc::lex::SourceCodeRange range, nodes::Edges h, T&& maybe_var);

    // Appends children to nodes at initialisation time.
    // These must have already been type checked.
    void append_unchecked(OptionalIndex parent, OptionalIndex child);

    template <class EnterNode, class BeforeChildren, class AfterChildren, class ExitNode>
    void depth_first_traverse_impl(EnterNode& enter_node, BeforeChildren& before_children,
                                   AfterChildren& after_children, ExitNode& exit_node, size_t i, size_t depth = 0,
                                   size_t visited = 0);

    template <class EnterNode, class BeforeChildren, class AfterChildren, class ExitNode>
    void breadth_first_traverse_impl(EnterNode& enter_node, BeforeChildren& before_children,
                                     AfterChildren& after_children, ExitNode& exit_node, size_t i, size_t depth = 0,
                                     size_t visited = 0);
};

////////////////////////////////////////////////////////////////////////////////
//              Parser Graph implementations
////////////////////////////////////////////////////////////////////////////////


template <typename To, NodeFlag... Froms, typename... ARGS>
auto NodeGraph::cast(TypedIndex<Froms...> from_index, ARGS&&... args) -> typename To::IndexType {
    static_assert(sizeof...(Froms) == 1, "Cannot deduce from type.");
    using FromIndex = TypedIndex<Froms...>;
    using ToIndex = typename To::IndexType;
    using FromType = typename decltype(nodes::NodeCollection::get_node_type_from_index_type<FromIndex>())::type;
    using ToType = To;

    static_assert(ToType::type == FromType::type, "Types must be the same to do a node mutation.");

    using FromTuple = typename FromType::HeldType;
    using ToTuple = typename ToType::HeldType;
    static_assert(std::is_convertible_v<FromTuple, ToTuple>, "Cannot convert children.");
    get_payload(Index { *from_index }) = ToType { std::forward<ARGS>(args)... };
    return ToIndex { *from_index };
}


template <NodeFlag... OLDs, NodeFlag... NEWs>
TypedIndex<OLDs...> NodeGraph::subsituted(TypedIndex<OLDs...> old, TypedIndex<NEWs...> n) {
    using OldIndex = TypedIndex<OLDs...>;
    using NewIndex = TypedIndex<NEWs...>;
    static_assert(std::is_convertible_v<NewIndex, OldIndex>, "The new index cannot be used to replace the old.");

    if (auto cc = edges[*old].first_child) {
        std::vector<Index> to_delete;
        depth_first_traverse(cc, [&](Index d) { to_delete.push_back(d); });
        for (auto d : to_delete) {
            edges[*d] = {};
            dead_nodes.push_back(*d);
        }
    }

    edges[*old].first_child = edges[*n].first_child;
    payloads[*old] = std::move(payloads[*n]);
}

template <NodeFlag... Ps, NodeFlag... Cs>
TypedIndex<Ps...> NodeGraph::merge_list(TypedIndex<Ps...> parent, TypedIndex<Cs...> child_list) {
    using ParentIndex = TypedIndex<Ps...>;
    using ChildIndex = TypedIndex<Cs...>;
    using ParentType = typename decltype(nodes::NodeCollection::get_node_type_from_index_type<ParentIndex>())::type;
    using ChildType = typename decltype(nodes::NodeCollection::get_node_type_from_index_type<ChildIndex>())::type;

    static_assert(ChildType::type == NodeType::List, "Child must be a list type");
    static_assert(ParentType::type == NodeType::List, "Parent must be a list type");

    using ChildHeld = typename ChildType::ChildIndex;
    using ParentHeld = typename ParentType::ChildIndex;

    static_assert(std::is_convertible_v<ChildHeld, ParentHeld>, "The parent cannot hold all the possible child types.");

    auto& child_list_edges = edges[*child_list];

    append_unchecked(parent, child_list_edges.first_child);

    child_list_edges = {};
    dead_nodes.push_back(*child_list);

    return parent;
}


template <typename F> void NodeGraph::traverse_only_children(F&& f, Index i) {
    const auto& edge = edges[*i];
    if (edge.first_child) {
        for (OptionalIndex c = edge.first_child; c; c = edges[*c].next_sibling) {
            f(locations[*c], edges[*c], payloads[*c], Index { *c });
        }
    }
}

template <class EnterNode, class BeforeChildren, class AfterChildren, class ExitNode>
void NodeGraph::breadth_first_traverse(Index i, EnterNode&& enter_node, BeforeChildren&& before_children,
                                       AfterChildren&& after_children, ExitNode&& exit_node) {
    if (edges.empty())
        return;
    breadth_first_traverse_impl(i, enter_node, before_children, after_children, exit_node, *i, 0);
}

template <class EnterNode, class BeforeChildren, class AfterChildren, class ExitNode>
void NodeGraph::depth_first_traverse(Index i, EnterNode&& enter_node, BeforeChildren&& before_children,
                                     AfterChildren&& after_children, ExitNode&& exit_node) {
    if (edges.empty())
        return;
    depth_first_traverse_impl(enter_node, before_children, after_children, exit_node, *i, 0);
}

// Mostly useful for debugging purposes.
template <typename F> void NodeGraph::flat_walk(F&& f) const {
    const auto sz = edges.size();
    for (size_t i { 0 }; i < sz; ++i) {
        f(locations[i], edges[i], payloads[i], i);
    }
}

template <typename TypedIndexT> [[nodiscard]] bool NodeGraph::is_a(Index i) const noexcept {
    using PayloadT = typename decltype(nodes::NodeCollection::get_node_type_from_index_type<TypedIndexT>())::type;
    return std::get_if<PayloadT>(&payloads[*i]);
}

template <NodeFlag... Ts> [[nodiscard]] const auto& NodeGraph::get_payload(TypedIndex<Ts...> index) const {
    using PayloadT = typename decltype(nodes::NodeCollection::get_node_type_from_index_type<TypedIndex<Ts...>>())::type;
    return std::get<PayloadT>(payloads[*index]);
}

template <NodeFlag... Ts> [[nodiscard]] auto& NodeGraph::get_payload(TypedIndex<Ts...> index) {
    using PayloadT = typename decltype(nodes::NodeCollection::get_node_type_from_index_type<TypedIndex<Ts...>>())::type;
    return std::get<PayloadT>(payloads[*index]);
}

template <NodeFlag... Ps, NodeFlag... Cs>
TypedIndex<Ps...> NodeGraph::prepend_to_list(TypedIndex<Ps...> list, TypedIndex<Cs...> item) {
    using ParentIndex = TypedIndex<Ps...>;
    using ChildIndex = TypedIndex<Cs...>;
    using ParentType = typename decltype(nodes::NodeCollection::get_node_type_from_index_type<ParentIndex>())::type;
    static_assert(std::is_constructible_v<typename ParentType::ChildIndex, ChildIndex>,
                  "Attempting to add the wrong child type.");

    auto& list_edges = edges[*list];
    if (!list_edges.first_child) {
        append_unchecked(*list, item);
        return list;
    }

    const auto old_first_child = list_edges.first_child;
    auto& first_child_edges = edges[*old_first_child];

    // re-parent
    for (OptionalIndex c = item; c; c = edges[*c].next_sibling) {
        edges[*c].parent = list;
    }

    const auto last_index = first_child_edges.last_sibling;
    first_child_edges.last_sibling = {}; // delete

    const auto item_last = edges[*item].last_sibling ?: item;
    edges[*item].last_sibling = last_index;

    edges[*item_last].next_sibling = old_first_child;
    edges[*old_first_child].previous_sibling = item_last;

    edges[*list].first_child = item;

    return list;
}

template <NodeFlag... Ps, NodeFlag... Cs>
TypedIndex<Ps...> NodeGraph::append_to_list(TypedIndex<Ps...> p, TypedIndex<Cs...> c) {
    using ParentIndex = TypedIndex<Ps...>;
    using ChildIndex = TypedIndex<Cs...>;
    using ParentType = typename decltype(nodes::NodeCollection::get_node_type_from_index_type<ParentIndex>())::type;
    static_assert(std::is_constructible_v<typename ParentType::ChildIndex, ChildIndex>,
                  "Attempting to add the wrong child type.");

    append_unchecked(*p, c);
    return p;
}

template <NodeFlag... Ps, typename... Cs>
TypedIndex<Ps...> NodeGraph::append_to_list(TypedIndex<Ps...> p, sc::lex::SourceCodeRange range, Cs... cs) {
    get_location(*p) = range;
    (append_to_list(p, cs), ...);
    return p;
}

template <typename Node, class... CHILDREN>
[[nodiscard]] auto NodeGraph::create(Node n, sc::lex::SourceCodeRange loc, CHILDREN... children) {
    static_assert(nodes::NodeCollection::has_node<typename Node::IndexType>(),
                  "You have failed to add the node to the variant at the bottom of the file.");
    static_assert(!(std::is_convertible_v<CHILDREN, LexerToken> || ...),
                  "You have passed a lexer token instead of a grammar rule, you have used the wrong index to $N.");

    if constexpr (Node::type == NodeType::Node) {
        static_assert(sizeof...(CHILDREN) == Node::number_of_children,
                      "Not enough arguments have been passed when creating a node.");
        const auto i = typename Node::IndexType { create_impl(loc, {}, std::move(n)) };
        (append_unchecked(i, children), ...);
        return i;
    } else if constexpr (Node::type == NodeType::Terminal) {
        static_assert(sizeof...(CHILDREN) == 0, "Terminal nodes cannot have children.");
        const auto i = typename Node::IndexType { create_impl(loc, {}, std::move(n)) };
        return i;
    } else if constexpr (Node::type == NodeType::List) {
        const auto i = typename Node::IndexType { create_impl(loc, {}, std::move(n)) };
        (append_to_list(i, std::forward<CHILDREN>(children)), ...);
        return i;
    } else {
        static_assert(Node::type == NodeType::Node || Node::type == NodeType::Terminal || Node::type == NodeType::List,
                      "Nodes must inherit from one of the correct types.");
    }
}

// This is untyped. Instead use the IRNode's create method (it's a friend class).
template <typename T> Index NodeGraph::create_impl(sc::lex::SourceCodeRange range, nodes::Edges h, T&& maybe_var) {
    assert(locations.size() == edges.size());
    assert(locations.size() == payloads.size());

    if (dead_nodes.empty()) {
        const auto i = edges.size();
        locations.push_back(range);
        edges.push_back(h);
        payloads.push_back(nodes::NodeVariant { maybe_var });
        return i;
    } else {
        const auto i = dead_nodes.back();
        dead_nodes.pop_back();

        locations[i] = range;
        edges[i] = h;
        payloads[i] = nodes::NodeVariant { maybe_var };
        return i;
    }
}

template <class EnterNode, class BeforeChildren, class AfterChildren, class ExitNode>
void NodeGraph::depth_first_traverse_impl(EnterNode& enter_node, BeforeChildren& before_children,
                                          AfterChildren& after_children, ExitNode& exit_node, size_t i, size_t depth,
                                          size_t visited) {
    assert(visited < 999'999'999); // just a silly number to make sure we don't get stuck in a loop

    const auto& edge = edges[i];

    const auto call = [&](auto& f) {
        using F = decltype(f);

        if constexpr (std::is_invocable_v<F, const sc::lex::SourceCodeRange&, const nodes::Edges&,
                                          const nodes::NodeVariant&, size_t>) {
            f(locations[i], edge, payloads[i], depth);
        } else if constexpr (std::is_invocable_v<F, const sc::lex::SourceCodeRange&>) {
            f(locations[i]);
        } else if constexpr (std::is_invocable_v<F, const nodes::Edges&>) {
            f(edge);
        } else if constexpr (std::is_invocable_v<F, const nodes::NodeVariant&>) {
            f(payloads[i]);
        } else if constexpr (std::is_invocable_v<F, const sc::lex::SourceCodeRange&, const nodes::NodeVariant&,
                                                 size_t>) {
            f(locations[i], payloads[i], depth);
        } else if constexpr (std::is_invocable_v<F, size_t>) {
            f(depth);
        } else if constexpr (std::is_invocable_v<F>) {
            f();
        } else {
            assert(false);
            f(); // EnterNode-ExitNode must meet one of the above signatures.
        }
    };

    call(enter_node);

    if (edge.first_child) {
        call(before_children);
        depth_first_traverse_impl(enter_node, before_children, after_children, exit_node, *edge.first_child, depth + 1,
                                  visited + 1);
        call(after_children);
    }

    call(exit_node);

    // last sibling is only valid for the first node.

    if (edge.next_sibling)
        depth_first_traverse_impl(enter_node, before_children, after_children, exit_node, *edge.next_sibling, depth,
                                  visited + 1);
}


template <class EnterNode, class BeforeChildren, class AfterChildren, class ExitNode>
void NodeGraph::breadth_first_traverse_impl(EnterNode& enter_node, BeforeChildren& before_children,
                                            AfterChildren& after_children, ExitNode& exit_node, size_t i, size_t depth,
                                            size_t visited) {
    assert(visited < 999'999'999); // just a silly number to make sure we don't get stuck in a loop

    const auto& edge = edges[i];

    const auto call = [&](auto f) {
        using F = decltype(f);

        if constexpr (std::is_invocable_v<F, const sc::lex::SourceCodeRange&, const nodes::Edges&,
                                          const nodes::NodeVariant&, size_t>) {
            f(locations[i], edge, payloads[i], depth);
        } else if constexpr (std::is_invocable_v<F, const sc::lex::SourceCodeRange&>) {
            f(locations[i]);
        } else if constexpr (std::is_invocable_v<F, const nodes::Edges&>) {
            f(edge);
        } else if constexpr (std::is_invocable_v<F, const nodes::NodeVariant&>) {
            f(payloads[i]);
        } else if constexpr (std::is_invocable_v<F, const sc::lex::SourceCodeRange&, const nodes::NodeVariant&,
                                                 size_t>) {
            f(locations[i], payloads[i], depth);
        } else if constexpr (std::is_invocable_v<F>) {
            f();
        } else if constexpr (std::is_invocable_v<F, size_t>) {
            f(depth);
        } else if constexpr (std::is_invocable_v<F, Index>) {
            f(depth, Index { static_cast<Index::IndexType>(i) });
        } else {
            assert(false);
            f(); // EnterNode-ExitNode must meet one of the above signatures.
        }
    };


    call(enter_node);

    if (edge.last_sibling) {
        for (auto c = edge.next_sibling; c; c = edges[*c].next_sibling) {
            call(before_children);
            breadth_first_traverse_impl(enter_node, before_children, after_children, exit_node * c, depth, visited + 1);
            call(after_children);
        }
    }

    call(exit_node);

    if (edge.first_child)
        breadth_first_traverse_impl(enter_node, before_children, after_children, exit_node, *edge.first_child,
                                    depth + 1, visited + 1);
}
}
