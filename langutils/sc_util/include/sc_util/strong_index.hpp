#pragma once

#include <cassert>
#include <cstddef>
#include <functional>
#include <limits>
#include <type_traits>

namespace sc::util {

/// @brief An integer that is a different type for each TAG, so indexes of different things cannot be mixed up.
/// @tparam UNDERLYING The integer type stored.
/// @tparam TAG An unrelated type that makes this index distinct, can be undefined.
template <typename UNDERLYING, typename TAG> //
struct StrongIndex {
    using Tag = TAG;
    using Underlying = UNDERLYING;
    static_assert(std::is_integral_v<UNDERLYING>);

    /// @brief Converts any integer, with a cast.
    template <typename T> [[nodiscard]] static constexpr StrongIndex from(T t) {
        return StrongIndex { static_cast<Underlying>(t) };
    }

    /// @brief Wraps a raw value.
    constexpr StrongIndex(Underlying u): m_value(u) {}

    constexpr StrongIndex(StrongIndex&&) noexcept = default;
    constexpr StrongIndex(const StrongIndex&) noexcept = default;
    constexpr StrongIndex& operator=(StrongIndex&&) noexcept = default;
    constexpr StrongIndex& operator=(const StrongIndex&) noexcept = default;

    /// @brief The raw value.
    [[nodiscard]] constexpr Underlying operator*() const { return m_value; }
    /// @brief The raw value.
    [[nodiscard]] constexpr Underlying value() const { return m_value; }

    [[nodiscard]] constexpr bool operator==(StrongIndex<UNDERLYING, TAG> other) const {
        return m_value == other.m_value;
    }

    [[nodiscard]] constexpr bool operator!=(StrongIndex<UNDERLYING, TAG> other) const { return !(*this == other); }

private:
    Underlying m_value {};
};

/// @brief The index @p n after @p i. Asserts it does not overflow.
template <typename UNDERLYING, typename TAG>
[[nodiscard]] constexpr StrongIndex<UNDERLYING, TAG> advance(StrongIndex<UNDERLYING, TAG> i,
                                                             typename StrongIndex<UNDERLYING, TAG>::Underlying n = 1) {
    if constexpr (std::is_signed_v<UNDERLYING>)
        assert(*i >= 0 && n >= 0 && "a negative index or step");
    assert(n <= std::numeric_limits<UNDERLYING>::max() - *i && "the index overflows");
    return StrongIndex<UNDERLYING, TAG> { static_cast<UNDERLYING>(*i + n) };
}

/// @brief The index @p n before @p i. Asserts it does not underflow.
template <typename UNDERLYING, typename TAG>
[[nodiscard]] constexpr StrongIndex<UNDERLYING, TAG> retreat(StrongIndex<UNDERLYING, TAG> i,
                                                             typename StrongIndex<UNDERLYING, TAG>::Underlying n = 1) {
    if constexpr (std::is_signed_v<UNDERLYING>)
        assert(*i >= 0 && n >= 0 && "a negative index or step");
    assert(n <= *i && "the index underflows");
    return StrongIndex<UNDERLYING, TAG> { static_cast<UNDERLYING>(*i - n) };
}

/// @brief The indexes from first up to, not including, last, for a range-for, or counting down from last when reversed.
/// @tparam Index A StrongIndex.
/// @tparam Reverse Whether to count down.
template <typename Index, bool Reverse = false> class IndexRange {
    using Underlying = typename Index::Underlying;

public:
    class Iterator {
    public:
        explicit constexpr Iterator(Underlying v): m_value(v) {}
        [[nodiscard]] constexpr Index operator*() const {
            return Index { static_cast<Underlying>(Reverse ? m_value - 1 : m_value) };
        }
        constexpr Iterator& operator++() {
            if constexpr (Reverse)
                --m_value;
            else
                ++m_value;
            return *this;
        }
        [[nodiscard]] constexpr bool operator!=(Iterator other) const { return m_value != other.m_value; }

    private:
        Underlying m_value;
    };

    /// @param first The first index.
    /// @param last One past the final index.
    constexpr IndexRange(Index first, Index last): m_first(*first), m_last(*last) {}

    [[nodiscard]] constexpr Iterator begin() const { return Iterator { Reverse ? m_last : m_first }; }
    [[nodiscard]] constexpr Iterator end() const { return Iterator { Reverse ? m_first : m_last }; }
    /// @brief The same indexes in the opposite order.
    [[nodiscard]] constexpr IndexRange<Index, !Reverse> reversed() const {
        return { Index { m_first }, Index { m_last } };
    }

private:
    Underlying m_first;
    Underlying m_last;
};

/// @brief The indexes from @p first up to, not including, @p last.
template <typename UNDERLYING, typename TAG>
[[nodiscard]] constexpr IndexRange<StrongIndex<UNDERLYING, TAG>> range(StrongIndex<UNDERLYING, TAG> first,
                                                                       StrongIndex<UNDERLYING, TAG> last) {
    return { first, last };
}

/// @brief The value StrongOptionalIndex uses for empty: the maximum for unsigned types, and -1 for signed ones.
template <typename T> [[nodiscard]] static constexpr T default_invalid() {
    static_assert(std::is_integral_v<T>);
    if constexpr (std::is_unsigned_v<T>) {
        return std::numeric_limits<T>::max();
    } else {
        return -1;
    }
}

/// @brief A StrongIndex that may be empty.
/// @tparam UNDERLYING The integer type stored.
/// @tparam TAG An unrelated type that makes this index distinct, can be undefined.
/// @tparam INVALID_VALUE The value that means empty.
template <typename UNDERLYING, typename TAG, UNDERLYING INVALID_VALUE = default_invalid<UNDERLYING>()> //
struct StrongOptionalIndex {
    using Tag = TAG;
    using Underlying = UNDERLYING;
    static_assert(std::is_integral_v<Underlying>);

    /// @brief Creates an empty index.
    constexpr StrongOptionalIndex(): m_value(INVALID_VALUE) {}
    constexpr StrongOptionalIndex(Underlying u): m_value(u) {}
    constexpr StrongOptionalIndex(StrongIndex<UNDERLYING, TAG> u): m_value(*u) {}

    constexpr StrongOptionalIndex(const StrongOptionalIndex&) noexcept = default;
    constexpr StrongOptionalIndex(StrongOptionalIndex&&) noexcept = default;
    constexpr StrongOptionalIndex& operator=(const StrongOptionalIndex&) noexcept = default;
    constexpr StrongOptionalIndex& operator=(StrongOptionalIndex&&) noexcept = default;

    [[nodiscard]] constexpr Underlying operator*() const { return m_value; }
    [[nodiscard]] constexpr Underlying value() const { return m_value; }
    /// @brief Whether the index is not empty.
    [[nodiscard]] constexpr explicit operator bool() const { return m_value != INVALID_VALUE; }

    [[nodiscard]] constexpr bool operator==(StrongOptionalIndex<UNDERLYING, TAG, INVALID_VALUE> other) const {
        return m_value == other.m_value;
    }
    [[nodiscard]] constexpr bool operator!=(StrongOptionalIndex<UNDERLYING, TAG, INVALID_VALUE> other) const {
        return !(*this == other);
    }

private:
    Underlying m_value { INVALID_VALUE };
};

/// @brief Whether @p T and @p Y are the optional and non optional forms of the same index.
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

namespace std {


template <typename UNDERLYING, typename TAG> //
struct hash<sc::util::StrongIndex<UNDERLYING, TAG>> {
    using T = sc::util::StrongIndex<UNDERLYING, TAG>;
    std::size_t operator()(const T& t) const { return std::hash<UNDERLYING> {}(*t); }
};

template <typename UNDERLYING, typename TAG, UNDERLYING INVALID> //
struct hash<sc::util::StrongOptionalIndex<UNDERLYING, TAG, INVALID>> {
    using T = sc::util::StrongOptionalIndex<UNDERLYING, TAG, INVALID>;
    std::size_t operator()(const T& t) const { return std::hash<UNDERLYING> {}(*t); }
};
}
