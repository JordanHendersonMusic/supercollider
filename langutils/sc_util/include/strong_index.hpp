#pragma once

#include <cstddef>
#include <functional>
#include <type_traits>

namespace sc::util {

template <typename UNDERLYING, typename TAG> struct StrongIndex {
    using Tag = TAG;
    using Underlying = UNDERLYING;
    static_assert(std::is_integral_v<UNDERLYING>);

    constexpr StrongIndex(Underlying u): m_value(u) {}

    [[nodiscard]] constexpr Underlying operator*() const { return m_value; }
    [[nodiscard]] constexpr Underlying value() const { return m_value; }

    struct Hasher {
        std::size_t operator()(const StrongIndex<Underlying, TAG>& k) const { return std::hash<Underlying>()(*k); }
    };

private:
    Underlying m_value;
};

template <typename UNDERLYING, typename TAG, UNDERLYING INVALID_VALUE> struct StrongOptionalIndex {
    using Tag = TAG;
    using Underlying = UNDERLYING;
    static_assert(std::is_integral_v<Underlying>);

    constexpr StrongOptionalIndex(): m_value(INVALID_VALUE) {}
    constexpr StrongOptionalIndex(Underlying u): m_value(u) {}
    constexpr StrongOptionalIndex(StrongIndex<UNDERLYING, TAG> u): m_value(*u) {}


    constexpr StrongOptionalIndex(const StrongOptionalIndex&) noexcept = default;
    constexpr StrongOptionalIndex(StrongOptionalIndex&&) noexcept = default;
    constexpr StrongOptionalIndex& operator=(const StrongOptionalIndex&) noexcept = default;
    constexpr StrongOptionalIndex& operator=(StrongOptionalIndex&&) noexcept = default;

    [[nodiscard]] constexpr Underlying operator*() const { return m_value; }
    [[nodiscard]] constexpr Underlying value() const { return m_value; }
    [[nodiscard]] constexpr operator bool() const { return m_value != INVALID_VALUE; }

    struct Hasher {
        std::size_t operator()(const StrongIndex<Underlying, TAG>& k) const { return std::hash<Underlying>()(*k); }
    };

private:
    Underlying m_value;
};

template <typename T, typename Y> [[nodiscard]] static constexpr bool is_a_pair() {
    if (std::is_same_v<T, Y>)
        return false;
    if (!std::is_same_v<typename T::Tag, typename Y::Tag>)
        return false;
    if (!std::is_same_v<typename T::Underlying, typename Y::Underlying>)
        return false;
    return std::is_convertible_v<T, bool> || std::is_convertible_v<Y, bool>;
}

}
