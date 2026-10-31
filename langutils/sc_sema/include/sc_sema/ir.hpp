#pragma once


#include "sc_util/indexed_vector.hpp"
#include "sc_util/typed_graph.hpp"

#include "sc_sema/details/indexes.hpp"
#include "sc_sema/details/node_collection.hpp"

#include <memory>
#include <string_view>
#include <unordered_map>
#include <utility>

namespace sc::src {
struct TextInfo;
}


namespace sc::ir {


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////

}

namespace sc::util::typed_graph {

extern template struct sc::util::typed_graph::GraphImplementation<sc::ir::OptNodeIndex, sc::ir::NodeIndex,
                                                                  sc::ir::NodeCollectionHelper>;
}


namespace sc::ir {

/// @brief An argument or variable (including in generators).
struct LocalVariable {
    SymSCIdentifier name; ///< The variable's name.
    SCLocalOwner_I owner; ///< The function, method, or generator that declares it.
    SourceLocation declaration; ///< Where it is declared.
    bool assigned_to { false }; ///< Whether it is assigned after it is declared. The declaration doesn't count.
    bool captured { false }; ///< Whether a function other than its owner uses it.
};

/// @brief An instance variable, class variable, or constant. There is one IRGraph per class.
struct MemberVariable {
    SymSCIdentifier name; ///< The variable's name.
    SymSCClassName owner; ///< The class that declares it.
    SourceLocation declaration; ///< Where it is declared.
};

/// @brief The intermediate representation of one class, or of one piece of interpreted code.
///
/// A typed graph of nodes (see ir_nodes.hpp), each with the SourceLocation it came from, plus the tables of local
/// variables, member variables, sources, and constant nodes. It holds the text its locations refer to, so the ast is
/// not needed once it is lowered. Walk it type safely with children() and the typed indexes.
struct IRGraph : private sc::util::typed_graph::GraphImplementation<OptNodeIndex, NodeIndex, NodeCollectionHelper> {
    using Base = sc::util::typed_graph::GraphImplementation<OptNodeIndex, NodeIndex, NodeCollectionHelper>;

public:
    IRGraph() = default;
    IRGraph(const IRGraph&) = default;
    IRGraph(IRGraph&&) noexcept = default;
    IRGraph& operator=(const IRGraph&) = delete;
    IRGraph& operator=(IRGraph&&) = delete;

    using Edges = Base::Edges;
    using Variant = Base::Variant;

    template <typename I> using NodeTypeFromIndex = Base::NodeTypeFromIndex<I>;

    ////////////////////////////////////////////////////////////////////////////////
    // Create
    ////////////////////////////////////////////////////////////////////////////////
    /// @brief Adds a node to the graph.
    /// @param node The node, which decides the type of the index returned.
    /// @param location Where the node came from.
    /// @param child_indexes The node's children, which must match what the node type accepts.
    /// @return The index of the new node.
    template <typename Node, typename... ChildrenIndexes>
    [[nodiscard]] typename Node::Index create(Node&& node, SourceLocation location, ChildrenIndexes&&... child_indexes);

    ////////////////////////////////////////////////////////////////////////////////
    // Creating Edges
    ////////////////////////////////////////////////////////////////////////////////
    using Base::append;

    ////////////////////////////////////////////////////////////////////////////////
    // node type operations
    ////////////////////////////////////////////////////////////////////////////////
    using Base::is_a;

    /// @brief The readable name of the node type an index refers to.
    template <typename TypedIndex> [[nodiscard]] static constexpr const char* node_name(TypedIndex);
    /// @brief The readable name of the node type held by a variant.
    [[nodiscard]] static const char* node_name(const Variant& variant);

    /// @name Roots
    /// A graph has one root: a class, an expression, or interpreted code.
    /// @{

    /// @brief Sets the root, replacing any previous one.
    void add_root(SCClass_I c);
    void add_root(SCAnyExpr_I c);
    void add_root(SCInterpreted_I c);

    /// @brief The root, empty if none has been added.
    std::optional<std::variant<SCClass_I, SCAnyExpr_I, SCInterpreted_I>> root() const;
    /// @}

    ////////////////////////////////////////////////////////////////////////////////
    // accessors
    ////////////////////////////////////////////////////////////////////////////////
    using Base::payload;
    /// @brief The first child of a node.
    /// To walk one node's children without its siblings, depth_first_traverse continues along the siblings.
    [[nodiscard]] OptNodeIndex first_child(NodeIndex i) const { return Base::edges(i).first_child; }
    using Base::last_child;
    /// @brief Where the node came from.
    [[nodiscard]] SourceLocation location(NodeIndex i) const { return m_node_locations[i]; }

    ////////////////////////////////////////////////////////////////////////////////
    // Sources
    // The text the locations refer to. Holding the text keeps it alive, the ast isn't needed once it is lowered.
    ////////////////////////////////////////////////////////////////////////////////
    /// @brief The index of a source text, adding it to the table if it isn't already there.
    [[nodiscard]] SourceIndex register_source(const std::shared_ptr<const src::TextInfo>& text);
    /// @brief A location in a source text, registering the text if needed.
    [[nodiscard]] SourceLocation make_location(const std::shared_ptr<const src::TextInfo>& text,
                                               src::SourceCodeRange range);
    /// @brief The text of a source.
    [[nodiscard]] const std::shared_ptr<const src::TextInfo>& text_info(SourceIndex i) const;
    /// @brief Every source text the locations refer to.
    [[nodiscard]] const util::IndexedVector<SourceIndex, std::shared_ptr<const src::TextInfo>>& sources() const;
    /// @brief The range in the source of a location.
    [[nodiscard]] static src::SourceCodeRange range(SourceLocation l);
    /// @brief The source text a location covers.
    [[nodiscard]] std::string_view read(SourceLocation l) const;


    ////////////////////////////////////////////////////////////////////////////////
    // Children. This is the type safe way to walk the graph.
    ////////////////////////////////////////////////////////////////////////////////
    using Base::as_a;
    using Base::assume_a;
    using Base::children;

    ////////////////////////////////////////////////////////////////////////////////
    // Traversal
    ////////////////////////////////////////////////////////////////////////////////

    /// @brief A callback for depth_first_traverse() that does nothing.
    struct Default {
        void operator()(const Edges&, const Variant&, const SourceLocation&, NodeIndex, size_t /**depth*/) {}
    };

    /// @brief Walks the tree from a node, depth first.
    /// All the callbacks have the signature
    /// `(const Edges&, const Variant&, const SourceLocation&, NodeIndex, size_t depth)`.
    /// Siblings after @p i are visited too, as a list is walked by looping rather than recursing.
    /// @param i The node to start at.
    /// @param enter_node Called when a node is reached.
    /// @param before_children Called once, before the children of a node that has any.
    /// @param after_children Called once, after the children of a node that has any.
    /// @param exit_node Called when a node, and all its children, are done.
    template <typename EnterNode, typename BeforeChildren = Default, typename AfterChildren = Default,
              typename ExitNode = Default>

    void depth_first_traverse(NodeIndex i, EnterNode enter_node, BeforeChildren before_children = {},
                              AfterChildren after_children = {}, ExitNode exit_node = {}) const;


    ////////////////////////////////////////////////////////////////////////////////
    // Local variables
    ////////////////////////////////////////////////////////////////////////////////
    /// @brief Adds a local variable to the table.
    /// @return Its index.
    [[nodiscard]] LocalIndex add_local(LocalVariable v) { return m_locals.push_back(v); }
    /// @brief A local variable.
    [[nodiscard]] const LocalVariable& local(LocalIndex i) const { return m_locals[i]; }
    [[nodiscard]] LocalVariable& local(LocalIndex i) { return m_locals[i]; }
    /// @brief Every local variable.
    [[nodiscard]] const util::IndexedVector<LocalIndex, LocalVariable>& locals() const { return m_locals; }

    ////////////////////////////////////////////////////////////////////////////////
    // Member variables
    ////////////////////////////////////////////////////////////////////////////////
    /// @brief The index of a member variable, adding it to the table if it isn't already there.
    [[nodiscard]] MemberIndex member_index(MemberVariable v);
    /// @brief A member variable.
    [[nodiscard]] const MemberVariable& member(MemberIndex i) const { return m_members[i]; }
    /// @brief Every member variable.
    [[nodiscard]] const util::IndexedVector<MemberIndex, MemberVariable>& members() const { return m_members; }

    ////////////////////////////////////////////////////////////////////////////////
    // Constexprs
    ////////////////////////////////////////////////////////////////////////////////

    /// @brief Records that a node always evaluates to a constant.
    void register_node_as_consteval(NodeIndex node, ConstExprIndex expr);

    /// @brief The constant a node always evaluates to, empty if it is not known to be one.
    [[nodiscard]] std::optional<ConstExprIndex> node_as_constexpr(NodeIndex node) const;


private:
    util::IndexedVector<NodeIndex, SourceLocation> m_node_locations {};
    util::IndexedVector<SourceIndex, std::shared_ptr<const src::TextInfo>> m_sources {};
    util::IndexedVector<LocalIndex, LocalVariable> m_locals {};
    util::IndexedVector<MemberIndex, MemberVariable> m_members {};

    std::unordered_map<NodeIndex, ConstExprIndex> nodes_to_constants {};
    std::variant<std::monostate, SCClass_I, SCAnyExpr_I, SCInterpreted_I> m_roots;
};


////////////////////////////////////////////////////////////////////////////////

template <typename TypedIndex> [[nodiscard]] constexpr const char* IRGraph::node_name(TypedIndex) {
    using NodeType = NodeTypeFromIndex<TypedIndex>;
    return NodeType::type_name;
};

template <typename Node, typename... ChildrenIndexes>
[[nodiscard]] typename Node::Index IRGraph::create(Node&& node, SourceLocation location,
                                                   ChildrenIndexes&&... child_indexes) {
    const auto index = Base::create(std::forward<Node>(node), std::forward<ChildrenIndexes>(child_indexes)...);
    if (m_node_locations.contains(index)) {
        m_node_locations[index] = std::move(location);
    } else {
        assert(index == m_node_locations.next_index());
        (void)m_node_locations.push_back(std::move(location));
    }

    return index;
}

template <typename EnterNode, typename BeforeChildren, typename AfterChildren, typename ExitNode>
void IRGraph::depth_first_traverse(NodeIndex i, EnterNode enter_node, BeforeChildren before_children,
                                   AfterChildren after_children, ExitNode exit_node) const {
    const auto wrap = [&](auto& f) {
        return [&](const Edges& e, const Variant& v, NodeIndex index, size_t d) {
            f(e, v, m_node_locations[index], index, d);
        };
    };
    Base::depth_first_traverse(i, wrap(enter_node), wrap(before_children), wrap(after_children), wrap(exit_node));
};
}
