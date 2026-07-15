// Copyright Jordan Henderson 2026
#pragma once

#include <type_traits>
#include <variant>
#include <tuple>
#include <utility>

#include "text_location.hpp"
#include "index.hpp"

namespace sc::parser {

namespace graph {
class ParserGraph;
}

namespace nodes::priv {

template <class IndexT> struct NodeBase {
    // The index type used to represent this node.
    using IndexType = IndexT;

    template <class T, class ThisTypedIndex, typename... CHILD_INDEXES>
    ThisTypedIndex create(graph::ParserGraph&, sc::lex::SourceCodeRange, CHILD_INDEXES&&... indexes) &&;
};


template <typename T, class ThisTypedIndex> struct TerminalNode : public NodeBase<ThisTypedIndex> {
    static constexpr auto number_of_children { 0 };

    ThisTypedIndex create(graph::ParserGraph& g, sc::lex::SourceCodeRange range) && {
        return static_cast<NodeBase<ThisTypedIndex>&&>(std::move(*this)).template create<T, ThisTypedIndex>(g, range);
    }
};

template <typename T, class ThisTypedIndex, typename... CHILD_INDEXES> struct Node;

template <typename T, class ThisTypedIndex, typename... CHILD_INDEXES>
struct Node<T(CHILD_INDEXES...), ThisTypedIndex> : public NodeBase<ThisTypedIndex> {
    static_assert((std::is_convertible_v<CHILD_INDEXES, MaybeIndex> && ...), "Should only takes Indexs as the ARGS");

    static_assert(sizeof...(CHILD_INDEXES) > 0, "Use TerminalNode instead.");

    static constexpr auto number_of_children { sizeof...(CHILD_INDEXES) };

    ThisTypedIndex create(graph::ParserGraph& g, sc::lex::SourceCodeRange range, CHILD_INDEXES&&... indexes) && {
        return static_cast<NodeBase<ThisTypedIndex>&&>(*this).template create<T, ThisTypedIndex>(g, range, indexes...);
    }
};


template <typename T, class ThisTypedIndex, typename ChildIndexT> struct ListNode : public NodeBase<ThisTypedIndex> {
    static_assert((std::is_convertible_v<ChildIndexT, MaybeIndex>), "Should only takes Indexs as the ARGS");

    using ChildIndex = ChildIndexT;

    template <class... CS> ThisTypedIndex create(graph::ParserGraph& g, sc::lex::SourceCodeRange range, CS&&... cs) &&;
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

private:
    // For some reason clang-format confuses clang-d (funny since they are both clang!)
    // clang-format off
    template <size_t CurrentI, class IndexT, typename IndexT2 = std::enable_if_t<CurrentI<sizeof...(Ts), IndexT>> 
    constexpr static auto get_node_type_from_index_type_impl() {
        using CurrentT = std::tuple_element_t<CurrentI, tuple>;
        if constexpr (std::is_same_v<typename CurrentT::IndexType, IndexT2>) {
            return TypeWrapper<CurrentT> { };
        } else {
            static_assert(CurrentI + 1 < sizeof...(Ts), "Could not find type");
            return get_node_type_from_index_type_impl<CurrentI + 1, IndexT2>();
        }
    }
    // clang-format on

    struct GetNameVisitor {
        template <class T> [[nodiscard]] constexpr const char* operator()(const T&) const noexcept { return T::name; }
    };
};


} // ir::nodes::priv

} // sc::parser
