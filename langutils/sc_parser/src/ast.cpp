// Copyright Jordan Henderson 2026
#include "sc_parser/ast.hpp"

namespace sc::ast {

void ASTGraph::add_diagnostic(sc::diag::Diagnostic d) {
    m_has_fatal_diagnostic |= d.fatal();
    m_diagnostics.push_back(std::move(d));
}

[[nodiscard]] const std::vector<sc::diag::Diagnostic>& ASTGraph::diagnostics() const& { return m_diagnostics; }

[[nodiscard]] std::vector<sc::diag::Diagnostic> ASTGraph::diagnostics() && { return std::move(m_diagnostics); }

RegionListIndex ASTGraph::assign_root(RegionListIndex i) {
    m_roots = i;
    return i;
}

ClassOrExtensionListIndex ASTGraph::assign_root(ClassOrExtensionListIndex i) {
    m_roots = i;
    return i;
}

[[nodiscard]] std::optional<Index> ASTGraph::root_any() const {
    return std::visit(priv::overload {
                          [](std::monostate) -> std::optional<Index> { return std::nullopt; },
                          [](auto a) -> std::optional<Index> { return Index { *a }; },
                      },
                      m_roots);
}

template <typename... Ts> struct overload : Ts... {
    using Ts::operator()...;
};
template <typename... Ts> overload(Ts... ts) -> overload<Ts...>;

[[nodiscard]] std::optional<std::variant<RegionListIndex, ClassOrExtensionListIndex>> ASTGraph::root() const {
    using R = std::optional<std::variant<RegionListIndex, ClassOrExtensionListIndex>>;
    return std::visit(overload {
                          [](std::monostate) -> R { return std::nullopt; },
                          [](auto i) -> R { return { i }; },
                      },
                      m_roots);
};


[[nodiscard]] ASTGraph::operator bool() const { return !m_has_fatal_diagnostic; }


} // sc::parser::graph
