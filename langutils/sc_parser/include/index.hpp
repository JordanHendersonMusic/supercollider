// Copyright Jordan Henderson 2026
#pragma once
#include <array>
#include <cassert>
#include <cstddef>
#include <limits>
#include <optional>
#include <type_traits>
#include "node_flags.hpp"

namespace sc::parser {

class Index;

class MaybeIndex {
public:
    using V = std::size_t;
    static constexpr auto invalid_value = std::numeric_limits<V>::max();

    constexpr MaybeIndex(V v) noexcept: m_value(v) {}
    constexpr MaybeIndex() noexcept: m_value(invalid_value) {}

    [[nodiscard]] constexpr explicit operator bool() const { return m_value != invalid_value; }

    [[nodiscard]] constexpr V value() const noexcept { return m_value; }
    [[nodiscard]] constexpr V operator()() const noexcept { return m_value; }
    [[nodiscard]] constexpr V operator*() const { return value(); }

    [[nodiscard]] constexpr operator std::optional<Index>();

private:
    V m_value;
};

// Index can cast to a MaybeIndex freely.
class Index : public MaybeIndex {
public:
    constexpr Index(MaybeIndex::V v) noexcept: MaybeIndex(v) {}
    constexpr Index() = delete; // can't default construct
};

[[nodiscard]] constexpr MaybeIndex::operator std::optional<Index>() {
    return *this ? std::optional<Index> { value() } : std::nullopt;
}

namespace priv {
template <NodeFlag M, NodeFlag... Os> [[nodiscard]] static constexpr bool contains() { return ((M == Os) || ...); }
}


// Here is a little compromise. TypedIndex ought to inherit from Index (as it should always be valid), however, the
// bison generated parser requires all semantic values be default constructable so this isn't possible.
template <NodeFlag... Ts> struct TypedIndex : public MaybeIndex {
    static constexpr auto Possible { std::array<NodeFlag, sizeof...(Ts)> { Ts... } };
    using MaybeIndex::MaybeIndex;
    using MaybeIndex::operator*;


    // This nasty sfinae thing just checks this is a sub set of the other...
    template <NodeFlag... Others, typename = std::enable_if_t<((priv::contains<Ts, Others...>()) && ...)>>
    // ... but it is just a conversion operator.
    [[nodiscard]] constexpr operator TypedIndex<Others...>() const {
        return TypedIndex<Others...> { **this };
    }

    // These can't be private because some compilers won't let you access them in the using statement.
    template <const auto& arr, typename Indices = std::make_index_sequence<arr.size()>> struct PrivateAppender;
    template <const auto& arr, std::size_t... I> struct PrivateAppender<arr, std::index_sequence<I...>> {
        using type = TypedIndex<Ts..., arr[I]...>;
    };

    template <class OtherMany> [[nodiscard]] static constexpr auto private_appendFunc() {
        return PrivateAppender<OtherMany::Possible> {};
    }

    template <class OtherMany> using append = typename decltype(private_appendFunc<OtherMany>())::type;

    static constexpr auto prOptional() { return private_appendFunc<TypedIndex<NodeFlag::Missing>>(); }
};


template <class T> using maybe = typename decltype(T::prOptional())::type;

namespace priv {

template <class A, class B> static constexpr auto append() { return A::template append<B>(); }

// typed wrapper that overloads operator+ to behave like append
template <class T> struct TypedIndexWrapper {
    using type = T;
    template <class O> [[nodiscard]] constexpr auto operator+(O) {
        return TypedIndexWrapper<typename T::template append<typename O::type>> {};
    }
};

template <class... Others> static constexpr auto joinImpl() { return (priv::TypedIndexWrapper<Others> {} + ...); }
}

// Appends a bunch of TypedIndexes together.
template <class... Js> using join = typename decltype(priv::joinImpl<Js...>())::type;


}
