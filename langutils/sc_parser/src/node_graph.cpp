// Copyright Jordan Henderson 2026
#include "node_graph.hpp"

namespace sc::parser::graph {

void NodeGraph::add_diagnostic(Diagnostic d) {
    m_has_fatal_diagnostic |= d.fatal();
    m_diagnostics.push_back(std::move(d));
}

[[nodiscard]] const std::vector<Diagnostic>& NodeGraph::diagnostics() const& { return m_diagnostics; }

[[nodiscard]] std::vector<Diagnostic> NodeGraph::diagnostics() && { return std::move(m_diagnostics); }

RegionListIndex NodeGraph::assign_root(RegionListIndex i) {
    m_roots = i;
    return i;
}

ClassOrExtensionListIndex NodeGraph::assign_root(ClassOrExtensionListIndex i) {
    m_roots = i;
    return i;
}

[[nodiscard]] std::optional<Index> NodeGraph::root_any() const {
    return std::visit(priv::overload {
                          [](std::monostate) -> std::optional<Index> { return std::nullopt; },
                          [](auto a) -> std::optional<Index> { return Index { *a }; },
                      },
                      m_roots);
}

[[nodiscard]] std::variant<std::monostate, RegionListIndex, ClassOrExtensionListIndex> NodeGraph::root() const {
    return m_roots;
};


[[nodiscard]] NodeGraph::operator bool() const { return !m_has_fatal_diagnostic; }


} // sc::parser::graph
