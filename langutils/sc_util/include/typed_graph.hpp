#pragma once

#include "strong_index.hpp"
#include "type_set_index.hpp"
#include "typed_graph_node.hpp"
#include <tuple>
#include <type_traits>
#include <variant>
#include <vector>

namespace sc::util::typed_graph {

template <class T> struct TypeWrapper { using type = T; };

struct TypedGraphHelperBase {};

template <typename... Nodes> struct TypedGraphHelper : TypedGraphHelperBase {
    static_assert((std::is_base_of_v<NodeBaseBase, Nodes> && ...));
    using Variant = std::variant<Nodes...>;
    using Tuple = std::tuple<Nodes...>;
    using IndexSequence = std::make_index_sequence<sizeof...(Nodes)>;
    using UntypedStrongIndex = typename std::tuple_element_t<0, Tuple>::Index::UnderlyingIndex;

    template <class ToFind> [[nodiscard]] static constexpr bool has_node() noexcept {
        return has_node_impl<0, std::remove_reference_t<std::remove_cv_t<ToFind>>>();
    }
    template <class IndexT> constexpr static auto get_node_type_from_index_type() {
        return get_node_type_from_index_type_impl<0, IndexT>();
    }

    template <typename F> constexpr static auto all_node_types() { return (F().template operator()<Nodes>() && ...); }

    template <typename F> constexpr static auto at_least_node_types() {
        return (F().template operator()<Nodes>() || ...);
    }

protected:
    // For some reason clang-format confuses clang-d (funny since they are both clang!)
    // clang-format off
    template <std::size_t CurrentI, class IndexT, typename IndexT2 = std::enable_if_t<CurrentI<sizeof...(Nodes), IndexT>> 
    [[nodiscard]] constexpr static auto get_node_type_from_index_type_impl() noexcept {
        using CurrentT = std::tuple_element_t<CurrentI, Tuple>;
        if constexpr (std::is_same_v<typename CurrentT::Index, IndexT2>) {
            return TypeWrapper<CurrentT> { };
        } else {
            static_assert(CurrentI + 1 < sizeof...(Nodes), "Could not find type. Either the type isn't in the variant, or you have failed to pass it to the graph helper.");
            return get_node_type_from_index_type_impl<CurrentI + 1, IndexT2>();
        }
    }

    template <std::size_t CurrentI, class ToFind> 
    [[nodiscard]] constexpr static bool has_node_impl() noexcept {
        using CurrentT = std::tuple_element_t<CurrentI, Tuple>;
        if constexpr (std::is_same_v<typename CurrentT::Index, ToFind>) {
            return true;
        } else if constexpr (CurrentI + 1 < sizeof...(Nodes)) {
            return has_node_impl<CurrentI + 1, ToFind>();
        } else {
            return false;
        }
    }
    // clang-format on
};

struct IsTrivial {
    template <typename T> [[nodiscard]] constexpr bool operator()() const {
        return std::is_trivially_move_constructible_v<
                   T> && std::is_trivially_destructible_v<T> && std::is_nothrow_move_constructible_v<T> && std::is_trivially_move_assignable_v<T> && std::is_nothrow_move_assignable_v<T>;
    }
};


template <typename OptNodeIndex, //
          typename Edges, //
          typename T //
          > //
struct Iter {
    static_assert(std::is_integral_v<typename OptNodeIndex::Underlying>);
    static_assert(std::is_convertible_v<OptNodeIndex, bool>);
    constexpr Iter(OptNodeIndex current, const std::vector<Edges>& edge): m_current(current), m_edges(edge) {}

    Iter& operator+=(int offset);
    [[nodiscard]] Iter operator++(int);
    Iter& operator++() { return this->operator+=(1); }
    [[nodiscard]] constexpr operator bool() const { return m_current; }

    [[nodiscard]] constexpr T operator*() const;

private:
    OptNodeIndex m_current;
    const std::vector<Edges>& m_edges;
};

template <typename OptNodeIndex, typename Edges, typename T> //
struct Container {
    constexpr Container(const std::vector<Edges>& edges, OptNodeIndex first): m_edges(edges), m_first(first) {}
    [[nodiscard]] Iter<OptNodeIndex, Edges, T> iter() { return Iter { m_first, m_edges }; }

private:
    const std::vector<Edges>& m_edges;
    OptNodeIndex m_first;
};

// Used when destructing children from a node.
struct NoChildren {};

/**
@brief
Essentially this is linked list but done with indicies.
Additionally, all the node construction is heavily typed.
Was this is setup, aside from the basic accessors, you only need 3 methods, create, visit, and children.
This is done in a struct of vectors approach.

This is designed to be inherited from so you can add your own implementation and expose protected methods with the using
declaration.

@param OptID the optional index type.
@param ID the index type.
@param A specialization of TypedGraphHelper.

*/
template <typename OptIndex, //
          typename Index, //
          typename Helper, //
          typename = std::enable_if<std::is_base_of_v<TypedGraphHelperBase, Helper>> //
          >
struct GraphImplementation {
public: // This is meant to be privately inherited from.
    ////////////////////////////////////////////////////////////////////////////////
    static_assert(Helper::template all_node_types<IsTrivial>());
    static_assert(sc::util::is_a_pair<OptIndex, Index>(),
                  "Should have an optional and an non-optional strong index here. ");
    static_assert(std::is_convertible_v<OptIndex, bool>, "Probably got OptIndex and Index the wrong way around.");
    static_assert(std::is_same_v<typename Helper::UntypedStrongIndex,
                                 OptIndex> || std::is_same_v<typename Helper::UntypedStrongIndex, Index>,
                  "Index types aren't the ones the nodes use.");

    template <typename OptNodeIndex, typename Edges, typename T> friend struct Container;
    template <typename OptNodeIndex, typename Edges, typename T> friend struct Iter;

    ////////////////////////////////////////////////////////////////////////////////
    using UntypedNodeIndex = typename Helper::UntypedStrongIndex;
    // Unfortunately, some times the node indexs need to be optional.
    static constexpr bool NodesAreOptional = std::is_same_v<UntypedNodeIndex, OptIndex>;

    // Additionally, the last_sibling is stored in a separate header as it is only needed when inserting, not when
    // iterating.
    struct Edges {
        OptIndex first_child, parent, next_sibling, prev_sibling;
    };

    ////////////////////////////////////////////////////////////////////////////////
    // The data.
    ////////////////////////////////////////////////////////////////////////////////

    std::vector<std::size_t> m_dead_nodes;
    std::vector<Edges> m_edges;
    std::vector<OptIndex> m_last_child;
    std::vector<typename Helper::Variant> m_node_payload;

    ////////////////////////////////////////////////////////////////////////////////
    // Create
    ////////////////////////////////////////////////////////////////////////////////

    /**
    @brief How nodes are made. This will return the typed index. If you have extra data, use the index to figure out
    where to insert it.
    @tparam Node A node,
    @tparam  ...Children the typed children indexs
     */
    template <typename Node, //
              typename... ChildrenIndexes, //
              typename = std::enable_if<Helper::template has_node<Node>()>, //
              typename = std::enable_if<(std::is_convertible_v<ChildrenIndexes, UntypedNodeIndex> && ...)> //
              >
    [[nodiscard]] typename Node::Index create(Node node, ChildrenIndexes&&... child_indexes) {
        static_assert(Node::template is_constructible<ChildrenIndexes...>(),
                      "The children index types do not match the constructor");
        const typename Node::Index index = [&]() {
            if (m_dead_nodes.empty()) {
                const auto index_raw = m_node_payload.size();
                m_node_payload.push_back(std::move(node));
                m_edges.push_back({});
                m_last_child.push_back({});
                return Node::Index(index_raw);
            } else {
                const auto index_raw = m_dead_nodes.back();
                m_dead_nodes.pop_back();
                m_node_payload[index_raw] = std::move(node);
                m_edges[index_raw] = {};
                m_last_child[index_raw] = {};
                return Node::Index(index_raw);
            }
        }();

        (append_to_parent_unchecked(index, std::forward<ChildrenIndexes>(child_indexes)), ...);
        return index;
    }


    ////////////////////////////////////////////////////////////////////////////////
    // accessors
    ////////////////////////////////////////////////////////////////////////////////
    template <typename TypedIndex, typename = std::enable_if<typed_index::is_type_set_index<TypedIndex>()>>
    [[nodiscard]] constexpr auto& payload(TypedIndex i) {
        using R = typename decltype(Helper::template get_node_type_from_index_type<TypedIndex>())::type;
        return std::get<R>(m_node_payload[*i]);
    }

    template <typename TypedIndex, typename = std::enable_if<typed_index::is_type_set_index<TypedIndex>()>>
    [[nodiscard]] constexpr const auto& payload(TypedIndex i) const {
        using R = typename decltype(Helper::template get_node_type_from_index_type<TypedIndex>())::type;
        return std::get<R>(m_node_payload[*i]);
        return m_node_payload[*i];
    }

    [[nodiscard]] constexpr typename Helper::Variant& payload(UntypedNodeIndex i) { return m_node_payload[*i]; };

    [[nodiscard]] constexpr const typename Helper::Variant& payload(UntypedNodeIndex i) const {
        return m_node_payload[*i];
    }

    [[nodiscard]] constexpr Edges& edges(UntypedNodeIndex i) { return m_edges[*i]; }
    [[nodiscard]] constexpr const Edges& edges(UntypedNodeIndex i) const { return m_edges[*i]; }


    ////////////////////////////////////////////////////////////////////////////////
    // Checks type of untyped node.
    ////////////////////////////////////////////////////////////////////////////////
    template <typename TypedIndex, typename = std::enable_if<typed_index::is_type_set_index<TypedIndex>()>>
    [[nodiscard]] constexpr bool is_a(UntypedNodeIndex i) const {
        using R = typename decltype(Helper::template get_node_type_from_index_type<TypedIndex>())::type;
        return std::get_if<R>(m_node_payload[*i]) != nullptr;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Return children of a node in a structured way.
    ////////////////////////////////////////////////////////////////////////////////
    template <typename TypedIndex, typename = std::enable_if<typed_index::is_type_set_index<TypedIndex>()>>
    [[nodiscard]] constexpr auto children(TypedIndex i) {
        using NodeType = typename decltype(Helper::template get_node_type_from_index_type<TypedIndex>())::type;
        if constexpr (NodeType::is_terminal())
            return NoChildren {};
        else if constexpr (NodeType::is_node()) {
            using Tup = typename NodeType::ChildrenIndexTupleType;
            return build_node_children_tuple<Tup>(i, std::make_index_sequence<std::tuple_size_v<Tup>>());
        } else {
            return Container<OptIndex, Edges, typename NodeType::HeldType> { m_edges, m_edges[*i].first_child };
        }
    }


private:
    /// [child] can be either a single node, or a list.
    void append_to_parent_unchecked(UntypedNodeIndex parent, UntypedNodeIndex child) {
        auto& parent_edges = m_edges[*parent];

        if (auto final_child = m_last_child[*parent]) {
            auto& f_c_edges = m_edges[*final_child];
            f_c_edges.next_sibling = child;
            m_edges[*child].prev_sibling = final_child;
        } else {
            parent_edges.first_child = child;
        }

        UntypedNodeIndex last_valid = child;
        for (OptIndex c = last_valid; c; c = m_edges[*c].next_sibling) {
            m_edges[*c].parent = parent;
            last_valid = *c;
        }
        m_last_child[*parent] = last_valid;
    }

    template <typename Tuple, size_t... IS>
    Tuple build_node_children_tuple(UntypedNodeIndex parent, std::index_sequence<IS...>) {
        auto first = m_edges[*parent].first_child;
        const auto postIncrement = [&](size_t) {
            auto out = first;
            first = m_edges[*first].next_sibling;
            return out;
        };

        // The order is well defined here because this is an initalized list.
        return Tuple { postIncrement(IS)... };
    };
};

////////////////////////////////////////////////////////////////////////////////
template <typename OptNodeIndex, typename Edges, typename T>
Iter<OptNodeIndex, Edges, T>& Iter<OptNodeIndex, Edges, T>::operator+=(int offset) {
    if (offset >= 0) {
        for (int i { 0 }; i < offset; ++i) {
            if (!m_current)
                break;
            m_current = m_edges[*m_current].next_sibling;
        }
    } else {
        for (int i { 0 }; i > offset; --i) {
            if (!m_current)
                break;
            m_current = m_edges[*m_current].prev_sibling;
        }
    }
    return *this;
}


template <typename OptNodeIndex, typename Edges, typename T>
[[nodiscard]] Iter<OptNodeIndex, Edges, T> Iter<OptNodeIndex, Edges, T>::operator++(int) {
    Iter copy = *this;
    if (m_current) {
        m_current = m_edges[*m_current].next_sibling;
    }
    return copy;
}

template <typename OptNodeIndex, typename Edges, typename T>
[[nodiscard]] constexpr T Iter<OptNodeIndex, Edges, T>::operator*() const {
    assert(m_current);
    return T { *m_current };
}
}
