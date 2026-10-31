#pragma once

#include <vector>

namespace sc::util::typed_graph {

/// @brief The links of a node: its first child, its parent, and its neighbours in its parent's list.
template <typename OptIndex> struct EdgesTemplate { OptIndex first_child, parent, next_sibling, prev_sibling; };

/// @brief Walks the children of a node, from the first to the last.
/// @tparam OptNodeIndex The untyped optional index of the nodes.
/// @tparam T The typed index each child is converted to.
template <typename OptNodeIndex, typename T> //
struct Iter {
    using Edges = EdgesTemplate<OptNodeIndex>;
    constexpr Iter(OptNodeIndex current, const std::vector<Edges>& edge): m_current(current), m_edges(edge) {}

    /// @brief Moves @p offset siblings along, backwards if negative. Stops at either end.
    Iter& operator+=(int offset);
    /// @brief Moves to the next sibling.
    /// @return The iterator from before it moved.
    [[nodiscard]] Iter operator++(int);
    /// @brief Moves to the next sibling.
    Iter& operator++() { return this->operator+=(1); }
    /// @brief Whether there is a current child, as opposed to having walked past the last.
    [[nodiscard]] constexpr explicit operator bool() const { return static_cast<bool>(m_current); }

    /// @brief The current child. There must be one.
    [[nodiscard]] constexpr T operator*() const;

private:
    OptNodeIndex m_current;
    const std::vector<Edges>& m_edges;
};

/// @brief The children of a node, which can be walked as many times as needed.
/// @tparam OptNodeIndex The untyped optional index of the nodes.
/// @tparam T The typed index each child is converted to.
template <typename OptNodeIndex, typename T> //
struct Container {
    using Edges = EdgesTemplate<OptNodeIndex>;
    constexpr Container(const std::vector<Edges>& edges, OptNodeIndex first): m_edges(edges), m_first(first) {}
    /// @brief An iterator at the first child.
    [[nodiscard]] Iter<OptNodeIndex, T> iter() const { return { m_first, m_edges }; }

private:
    const std::vector<Edges>& m_edges;
    OptNodeIndex m_first;
};

template <typename OptNodeIndex, typename T> Iter<OptNodeIndex, T>& Iter<OptNodeIndex, T>::operator+=(int offset) {
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


template <typename OptNodeIndex, typename T>
[[nodiscard]] Iter<OptNodeIndex, T> Iter<OptNodeIndex, T>::operator++(int) {
    Iter copy = *this;
    if (m_current) {
        m_current = m_edges[*m_current].next_sibling;
    }
    return copy;
}

template <typename OptNodeIndex, typename T> [[nodiscard]] constexpr T Iter<OptNodeIndex, T>::operator*() const {
    // assert(m_current);
    return T { *m_current };
}

}
