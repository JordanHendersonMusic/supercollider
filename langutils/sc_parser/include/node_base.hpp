// Copyright Jordan Henderson 2026
#pragma once

#include <type_traits>
#include <variant>
#include <tuple>
#include <utility>

#include "index.hpp"

namespace sc::parser {

namespace graph {
class NodeGraph;
}

enum struct NodeType { Terminal, Node, List };

namespace nodes::priv {

template <class IndexT> struct NodeBase {
    // The index type used to represent this node.
    using IndexType = IndexT;
};

template <typename T, class ThisTypedIndex> struct TerminalNode : public NodeBase<ThisTypedIndex> {
    static constexpr auto type { NodeType::Terminal };
    static constexpr auto number_of_children { 0 };

    using HeldType = void;
};

template <typename T, class ThisTypedIndex, typename... CHILD_INDEXES> struct Node;

template <typename T, class ThisTypedIndex, typename... CHILD_INDEXES>
struct Node<T(CHILD_INDEXES...), ThisTypedIndex> : public NodeBase<ThisTypedIndex> {
    static constexpr auto type { NodeType::Node };
    static_assert((std::is_convertible_v<CHILD_INDEXES, OptionalIndex> && ...), "Should only takes Indexs as the ARGS");

    static_assert(sizeof...(CHILD_INDEXES) > 0, "Use TerminalNode instead.");

    static constexpr auto number_of_children { sizeof...(CHILD_INDEXES) };
    using ChildrenIndexTupleType = std::tuple<CHILD_INDEXES...>;

    using HeldType = ChildrenIndexTupleType;
};


template <typename T, class ThisTypedIndex, typename ChildIndexT> struct ListNode : public NodeBase<ThisTypedIndex> {
    static constexpr auto type { NodeType::List };
    static_assert((std::is_convertible_v<ChildIndexT, OptionalIndex>), "Should only takes Indexs as the ARGS");

    using ChildIndex = ChildIndexT;

    using HeldType = ChildIndex;
};

template <class T> struct TypeWrapper { using type = T; };

template <typename... Ts> struct IRNodeCollectionHelper {
    using variant = std::variant<Ts...>;
    using tuple = std::tuple<Ts...>;
    using index_sequence = std::make_index_sequence<sizeof...(Ts)>;
    static_assert((std::is_convertible_v<decltype(Ts::name), const char*> && ...),
                  "All node types should have a static constexpr 'name' convertible to a const char*.");

    [[nodiscard]] static constexpr const char* get_name(const variant& v) noexcept {
        return std::visit(GetNameVisitor {}, v);
    }

    template <class IndexT> constexpr static auto get_node_type_from_index_type() {
        return get_node_type_from_index_type_impl<0, IndexT>();
    }


    template <class ToFind> [[nodiscard]] static constexpr bool has_node() noexcept {
        return has_node_impl<0, std::remove_reference_t<std::remove_cv_t<ToFind>>>();
    }

private:
    // For some reason clang-format confuses clang-d (funny since they are both clang!)
    // clang-format off
    template <size_t CurrentI, class IndexT, typename IndexT2 = std::enable_if_t<CurrentI<sizeof...(Ts), IndexT>> 
    [[nodiscard]] constexpr static auto get_node_type_from_index_type_impl() noexcept {
        using CurrentT = std::tuple_element_t<CurrentI, tuple>;
        if constexpr (std::is_same_v<typename CurrentT::IndexType, IndexT2>) {
            return TypeWrapper<CurrentT> { };
        } else {
            static_assert(CurrentI + 1 < sizeof...(Ts), "Could not find type. Either the type isn't in the variant, or you have failed to add it to the variant (at the bottom of the nodes.hpp file).");
            return get_node_type_from_index_type_impl<CurrentI + 1, IndexT2>();
        }
    }

    template <size_t CurrentI, class ToFind> 
    [[nodiscard]] constexpr static bool has_node_impl() noexcept {
        using CurrentT = std::tuple_element_t<CurrentI, tuple>;
        if constexpr (std::is_same_v<typename CurrentT::IndexType, ToFind>) {
            return true;
        } else if constexpr (CurrentI + 1 < sizeof...(Ts)) {
            return has_node_impl<CurrentI + 1, ToFind>();
        } else {
            return false;
        }
    }
    // clang-format on

    struct GetNameVisitor {
        template <class T> [[nodiscard]] constexpr const char* operator()(const T&) const noexcept { return T::name; }
    };
};


} // ir::nodes::priv

} // sc::parser
