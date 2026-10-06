#pragma once

#include "const_expr.hpp"
#include "ast_location.hpp"
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include "strong_index.hpp"
#include "type_set_index.hpp"
#include "typed_graph_node.hpp"
#include "const_expr.hpp"

namespace sc::ir {

template <typename T> struct Locatable {
    ASTLocation location;
    T value;
    [[nodiscard]] constexpr T& operator->() { return value; }
    [[nodiscard]] constexpr const T& operator->() const { return value; }

    [[nodiscard]] constexpr T& operator*() { return value; }
    [[nodiscard]] constexpr const T& operator*() const { return value; }
};

template <typename T> struct OptionallyLocatable {
    std::optional<ASTLocation> location;
    T value;

    [[nodiscard]] constexpr T& operator->() { return value; }
    [[nodiscard]] constexpr const T& operator->() const { return value; }

    [[nodiscard]] constexpr T& operator*() { return value; }
    [[nodiscard]] constexpr const T& operator*() const { return value; }
};

////////////////////////////////////////////////////////////////////////////////

enum struct SCNodeType {
    Missing,
    Message,
    Assign,

    KwArg,
    VariableDeclare,
    ExplicitReturn,

    KwArgList,
    PosArgList,
    VariadicArgList,
    Literal,
    Collection, // a collection that can't be turned into a literal.

    ArgumentDeclare,
    Function,
    Method,
    MethodList,
    Member,
    MemberList,
    Class,
    Extention,

    ExprList,
    ClassOrExtentionList,
};

////////////////////////////////////////////////////////////////////////////////

using NodeSpec = sc::util::typed_index::Spec<std::uint32_t, SCNodeType, struct Graph__>;
using NodeDef = util::typed_index::QuicklyDefineTypesFromSpec<NodeSpec>;
using OptNodeIndex = NodeDef::OptionalIndex;
using NodeIndex = NodeDef::Index;

template <SCNodeType... ts> using TypedNodeIndex = NodeDef::TypedIndex<ts...>;

template <typename... Ts> using join = sc::util::typed_index::join<Ts...>;
template <typename T> using maybe = join<T, TypedNodeIndex<SCNodeType::Missing>>;

////////////////////////////////////////////////////////////////////////////////

using SCLiteral_I = TypedNodeIndex<SCNodeType::Literal>;

using SCMessage_I = TypedNodeIndex<SCNodeType::Message>;
using SCAssign_I = TypedNodeIndex<SCNodeType::Assign>;

using SCKwArgList_I = TypedNodeIndex<SCNodeType::KwArgList>;
using SCPosArgList_I = TypedNodeIndex<SCNodeType::PosArgList>;
using SCVariadicArgList_I = TypedNodeIndex<SCNodeType::VariadicArgList>;

using SCKwArg_I = TypedNodeIndex<SCNodeType::KwArg>;
using SCExplicitReturn_I = TypedNodeIndex<SCNodeType::ExplicitReturn>;

using SCVariableDeclare_I = TypedNodeIndex<SCNodeType::VariableDeclare>;

using SCCollection_I = TypedNodeIndex<SCNodeType::Collection>;
using SCArgumentDeclare_I = TypedNodeIndex<SCNodeType::ArgumentDeclare>;
using SCFunction_I = TypedNodeIndex<SCNodeType::Function>;
using SCMethod_I = TypedNodeIndex<SCNodeType::Method>;
using SCMethodList_I = TypedNodeIndex<SCNodeType::MethodList>;
using SCMember_I = TypedNodeIndex<SCNodeType::Member>;
using SCMemberList_I = TypedNodeIndex<SCNodeType::MemberList>;
using SCClass_I = TypedNodeIndex<SCNodeType::Class>;
using SCExtention_I = TypedNodeIndex<SCNodeType::Extention>;

using SCExprList_I =
    join<TypedNodeIndex<SCNodeType::ExprList>, SCMethod_I, SCVariableDeclare_I, SCAssign_I, SCExplicitReturn_I>;

using SCClassOrExtentionList_I = TypedNodeIndex<SCNodeType::ClassOrExtentionList>;


// clang-format on
////////////////////////////////////////////////////////////////////////////////

namespace priv {
template <typename Index> using Terminal = sc::util::typed_graph_node::TerminalNode<Index>;
template <typename This, typename... ARGS> using Node = sc::util::typed_graph_node::Node<This, ARGS...>;
template <typename This, typename Child> using List = sc::util::typed_graph_node::ListNode<This, Child>;
}

////////////////////////////////////////////////////////////////////////////////

// This is just a place holder, you need to look it up in the
struct SCLiteral : priv::Terminal<SCLiteral_I> {
    static constexpr auto type_name { "SCLiteral" };
    ConstExprIndex index;
};

struct SCKwArg : priv::Node<SCKwArg_I(SCExprList_I value)> {
    static constexpr auto type_name { "SCKwArg" };
    Locatable<ConstSymbol> name;
};

struct SCPositionalArgs : priv::List<SCPosArgList_I, SCExprList_I> {
    static constexpr auto type_name { "SCPositionalArgs" };
    std::uint32_t size;
};

// This is a variable or a member assignment, it does not do `x.foo = 10`, which is the message `foo_(x, 10)`.
struct SCAssign : priv::Node<SCAssign_I(SCExprList_I value)> {
    static constexpr auto type_name { "SCAssign" };
    Locatable<ConstSymbol> name;
};

// clang-format off
struct SCMessage : priv::Node<
    SCMessage_I(
        SCExprList_I receiver, 
        maybe<SCPosArgList_I> positional_arguments,
        maybe<SCKwArgList_I> keyword_arguments,
        maybe<SCVariadicArgList_I> variadic_arguments, 
        maybe<SCExprList_I> adverb
    )
> {
    static constexpr auto type_name { "SCMessage" };
    // clang-format on
    Locatable<ConstSymbol> selector;
};

struct SCVariableDeclare : priv::Node<SCVariableDeclare_I(SCExprList_I value)> {
    static constexpr auto type_name { "SCVariableDeclare" };
    Locatable<ConstSymbol> name;
};

struct SCExplicitReturn : priv::Node<SCExplicitReturn_I(SCExprList_I value)> {
    static constexpr auto type_name { "SCExplicitReturn" };
};

struct SCCollection : priv::List<SCCollection_I, SCExprList_I> {
    static constexpr auto type_name { "SCCollection" };
    OptionallyLocatable<ConstClassName> collection_class;
    std::uint32_t size;
};


struct SCArgumentDeclare : priv::Node<SCArgumentDeclare_I(maybe<SCExprList_I>)> {
    static constexpr auto type_name { "SCArgumentDeclare" };
    ConstSymbol name;
    bool preserve_nil;
};

// clang-format off
struct SCFunction : priv::Node<
    SCFunction_I(
        maybe<SCArgumentDeclare_I> positional_arguments,
        maybe<SCArgumentDeclare_I> variadic_arguments, 
        maybe<SCExprList_I> body
    )
> {
    // clang-format on
    static constexpr auto type_name { "SCFunction" };
    std::optional<Locatable<ConstSymbol>> function_name;
};


// clang-format off
struct SCMethod : priv::Node<
    SCMethod_I(
        maybe<SCArgumentDeclare_I> positional_arguments,
        maybe<SCArgumentDeclare_I> variadic_arguments, 
        maybe<SCExprList_I> body
    )
> {
    // clang-format on
    static constexpr auto type_name { "SCMethod" };
    Locatable<ConstClassName> owning_class;
    std::optional<Locatable<ConstSymbol>> primitive;
};

struct SCMethodList : priv::List<SCMethodList_I, SCMethod_I> {
    static constexpr auto type_name { "SCMethodList" };
};


struct SCMember : priv::Node<SCMember_I(maybe<SCExprList_I> default_value)> {
    static constexpr auto type_name { "SCMember" };
    Locatable<ConstSymbol> name;
    bool read;
    bool write;
    bool constant;
};

struct SCMemberList : priv::List<SCMemberList_I, SCMember> {
    static constexpr auto type_name { "SCMemberList" };
};

// clang-format off
struct SCClass : priv::Node<
    SCClass_I(
        SCMemberList_I class_members, SCMemberList_I instance_members, 
        SCMethodList_I class_methods, SCMethodList_I instance_methods
    )
> {
    // clang-format on
    static constexpr auto type_name { "SCClass" };
    enum struct MemoryLayout { Float, Slot, Int };
    ConstClassName name;
    std::optional<Locatable<MemoryLayout>> MemoryLayout;
};

struct SCExtention : priv::Node<SCExtention_I(SCMethodList_I methods)> {
    static constexpr auto type_name { "SCExtention" };
    ConstClassName name;
};

struct SCClassOrExtentionList : priv::List<SCClassOrExtentionList_I, join<SCClass_I, SCExtention_I>> {
    static constexpr auto type_name { "SCClassOrExtentionList" };
};

struct SCExprList : priv::List<TypedNodeIndex<SCNodeType::ExprList>, SCExprList_I> {
    static constexpr auto type_name { "SCExprList" };
};

////////////////////////////////////////////////////////////////////////////////


// clang-format off
struct NodeCollection : sc::util::typed_graph_node::TypedGraphHelper<
    SCLiteral,
    SCKwArg, 
    SCPositionalArgs,
    SCAssign,
    SCMessage, 
    SCVariableDeclare, 
    SCExplicitReturn,
    SCCollection,
    SCArgumentDeclare,
    SCFunction,
    SCMethod,
    SCMethodList,
    SCMember,
    SCMemberList,
    SCClass,
    SCExtention,
    SCClassOrExtentionList,
    SCExprList
> {
    // clang-format on

    [[nodiscard]] static constexpr const char* get_name(const Variant& v) noexcept {
        return std::visit(GetNameVisitor {}, v);
    }

private:
    struct GetNameVisitor {
        template <class T> [[nodiscard]] constexpr const char* operator()(const T&) const noexcept {
            return T::type_name;
        }
    };
};


namespace iter {

template <typename T> struct Container;
template <typename T> struct Iter;

}

struct Graph {
    // clang-format off
    template < 
        typename T, 
        typename... ARGS, 
        typename = std::enable_if<NodeCollection::has_node<T>()>,
        typename = std::enable_if<(std::is_convertible_v<ARGS, NodeIndex> && ...)>
    >
    // clang-format on
    [[nodiscard]] typename T::Index create(T t, ASTLocation location, ARGS&&... args);

    template <SCNodeType... ts> [[nodiscard]] constexpr auto& payload(TypedNodeIndex<ts...> i);
    template <SCNodeType... ts> [[nodiscard]] constexpr const auto& payload(TypedNodeIndex<ts...> i) const;
    [[nodiscard]] constexpr NodeCollection::Variant& payload(NodeIndex i);
    [[nodiscard]] constexpr const NodeCollection::Variant& payload(NodeIndex i) const;

    template <typename T> [[nodiscard]] constexpr bool is_a(NodeIndex i) const;

    // Children can return the container you pass in a list node.
    // Otherwise it returns NoChildren if a terminal, or a tuple if it is a node.
    template <typename T> friend struct iter::Container;
    template <typename T> friend struct iter::Iter;
    struct NoChildren {};
    template <SCNodeType... ts> [[nodiscard]] constexpr auto children(TypedNodeIndex<ts...> i);

    [[nodiscard]] constexpr ASTLocation location(NodeIndex i) const { return locations[*i]; }


    template <class EnterNode, class BeforeChildren, class AfterChildren, class ExitNode>
    void depth_first_traverse(EnterNode& enter_node, BeforeChildren& before_children, AfterChildren& after_children,
                              ExitNode& exit_node, NodeIndex i);


private:
    struct Edges {
        OptNodeIndex first_child {}, parent {};
        OptNodeIndex next_sibling {}, prev_sibling {};
    };
    std::unordered_map<ConstClassName, SCClass_I, ConstClassName::Hasher> class_lookup;

    std::unordered_map<OptNodeIndex, ConstExprIndex, OptNodeIndex::Hasher> nodes_to_constants;

    std::vector<ConstExpression> constant_expressions;

    std::vector<std::size_t> dead_nodes;

    std::vector<Edges> edges;
    std::vector<OptNodeIndex> last_child;
    std::vector<NodeCollection::Variant> node_payload;
    std::vector<ASTLocation> locations;


    /// [child] can be either a single node, or a list.
    void append_to_parent_unchecked(NodeIndex parent, NodeIndex child);

    /// Requires the parent is a tuple and you've passed in the right tuple and index sequence.
    template <typename Tuple, size_t... IS>
    Tuple build_node_children_tuple(NodeIndex parent, std::index_sequence<IS...>);

    template <class EnterNode, class BeforeChildren, class AfterChildren, class ExitNode>
    void depth_first_traverse_impl(EnterNode& enter_node, BeforeChildren& before_children,
                                   AfterChildren& after_children, ExitNode& exit_node, NodeIndex i, size_t depth = 0,
                                   size_t visited = 0) const;
};


////////////////////////////////////////////////////////////////////////////////

namespace iter {

template <typename T> struct Iter {
    constexpr Iter(OptNodeIndex current, Graph& graph): m_current(current), m_graph(graph) {}
    Iter& operator+=(int offset) {
        if (offset >= 0) {
            for (int i { 0 }; i < offset; ++i) {
                if (!m_current)
                    break;
                m_current = m_graph.edges[*m_current].next_sibling;
            }
        } else {
            for (int i { 0 }; i > offset; --i) {
                if (!m_current)
                    break;
                m_current = m_graph.edges[*m_current].prev_sibling;
            }
        }
        return *this;
    }

    [[nodiscard]] Iter operator+(int offset) const {
        Iter copy = *this;
        if (offset >= 0) {
            for (int i { 0 }; i < offset; ++i) {
                if (!copy.m_current)
                    break;
                copy.m_current = m_graph.edges[*copy.m_current].next_sibling;
            }
        } else {
            for (int i { 0 }; i > offset; --i) {
                if (!copy.m_current)
                    break;
                copy.m_current = m_graph.edges[*copy.m_current].prev_sibling;
            }
        }
        return copy;
    }
    Iter& operator++() { return this->operator+=(1); }
    [[nodiscard]] constexpr operator bool() const { return m_current; }

    [[nodiscard]] constexpr T operator*() const {
        assert(m_current);
        return T { *m_current };
    }

private:
    OptNodeIndex m_current;
    Graph& m_graph;
};

template <typename T> struct Container {
    constexpr Container(Graph& graph, NodeIndex first): m_graph(graph), m_first(first) {}
    [[nodiscard]] Iter<T> begin() { return Iter<T> { m_first, m_graph }; }

private:
    Graph& m_graph;
    NodeIndex m_first;
};

}

////////////////////////////////////////////////////////////////////////////////

template <typename Tuple, size_t... IS>
inline Tuple Graph::build_node_children_tuple(NodeIndex parent, std::index_sequence<IS...>) {
    auto first = edges[*parent].first_child;
    const auto postIncrement = [&](size_t) {
        auto out = first;
        first = edges[*first].next_sibling;
        return out;
    };

    // The order is well defined here because this is an initalized list.
    return Tuple { postIncrement(IS)... };
}

template <class EnterNode, class BeforeChildren, class AfterChildren, class ExitNode>
inline void Graph::depth_first_traverse(EnterNode& enter_node, BeforeChildren& before_children,
                                        AfterChildren& after_children, ExitNode& exit_node, NodeIndex i) {
    depth_first_traverse_impl(enter_node, before_children, after_children, exit_node, i);
};

template <class EnterNode, class BeforeChildren, class AfterChildren, class ExitNode>
inline void Graph::depth_first_traverse_impl(EnterNode& enter_node, BeforeChildren& before_children,
                                             AfterChildren& after_children, ExitNode& exit_node, NodeIndex i,
                                             size_t depth, size_t visited) const {
    assert(visited < 999'999'999); // just a silly number to make sure we don't get stuck in a loop

    const auto call = [&](auto& f) { f(locations[*i], node_payload[*i], depth); };

    call(enter_node);
    const auto& edge = edges[*i];
    if (auto f = edge.first_child) {
        call(before_children);
        depth_first_traverse_impl(enter_node, before_children, after_children, edge, { *f }, depth + 1, visited + 1);
        call(after_children);
    }
    call(exit_node);

    if (auto n = edge.next_sibling)
        depth_first_traverse_impl(enter_node, before_children, after_children, exit_node, { *n }, depth, visited + 1);
}

template <SCNodeType... ts> [[nodiscard]] constexpr inline auto Graph::children(TypedNodeIndex<ts...> i) {
    using NodeType = typename decltype(NodeCollection::get_node_type_from_index_type<TypedNodeIndex<ts...>>())::type;
    if constexpr (NodeType::is_terminal())
        return NoChildren {};
    else if constexpr (NodeType::is_node()) {
        using Tup = typename NodeType::ChildrenIndexTupleType;
        return build_node_children_tuple<Tup>(i, std::make_index_sequence<std::tuple_size_v<Tup>>());
    } else {
        return iter::Container<typename NodeType::HeldType> { *this, edges[*i].first_child };
    }
}

template <typename Node, typename... Children, typename, typename>
[[nodiscard]] inline typename Node::Index Graph::create(Node node, ASTLocation location, Children&&... children) {
    static_assert(Node::template is_constructible<Children...>(),
                  "The children index types do not match the constructor");

    const typename Node::Index index = [&]() {
        if (dead_nodes.empty()) {
            const auto index_raw = node_payload.size();
            node_payload.push_back(std::move(node));
            edges.push_back({});
            last_child.push_back({});
            locations.push_back(location);
            return Node::Index(index_raw);
        } else {
            const auto index_raw = dead_nodes.back();
            dead_nodes.pop_back();
            node_payload[index_raw] = std::move(node);
            edges[index_raw] = {};
            last_child[index_raw] = {};
            locations[index_raw] = location;
            return Node::Index(index_raw);
        }
    }();

    (append_to_parent_unchecked(index, children), ...);
    return index;
}


template <SCNodeType... ts> [[nodiscard]] constexpr inline auto& Graph::payload(TypedNodeIndex<ts...> i) {
    using R = typename decltype(NodeCollection::get_node_type_from_index_type<TypedNodeIndex<ts...>>())::type;
    return std::get<R&>(node_payload[*i]);
}

template <SCNodeType... ts> [[nodiscard]] constexpr inline const auto& Graph::payload(TypedNodeIndex<ts...> i) const {
    using R = typename decltype(NodeCollection::get_node_type_from_index_type<TypedNodeIndex<ts...>>())::type;
    return std::get<const R&>(node_payload[*i]);
}

[[nodiscard]] constexpr inline NodeCollection::Variant& Graph::payload(NodeIndex i) { return node_payload[*i]; }

[[nodiscard]] constexpr inline const NodeCollection::Variant& Graph::payload(NodeIndex i) const {
    return node_payload[*i];
}

template <typename T> [[nodiscard]] constexpr inline bool Graph::is_a(NodeIndex i) const {
    return std::get_if<T>(payload(*i)) != nullptr;
}

inline void Graph::append_to_parent_unchecked(NodeIndex parent, NodeIndex child) {
    auto& parent_edges = edges[*parent];

    if (auto final_child = last_child[*parent]) {
        auto& f_c_edges = edges[*final_child];
        f_c_edges.next_sibling = child;
        edges[*child].prev_sibling = final_child;
    } else {
        parent_edges.first_child = child;
    }

    NodeIndex last_valid = child;
    for (OptNodeIndex c = last_valid; c; c = edges[*c].next_sibling) {
        edges[*c].parent = parent;
        last_valid = *c;
    }
    last_child[*parent] = last_valid;
}

}
