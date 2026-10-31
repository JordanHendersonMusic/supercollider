// Copyright Jordan Henderson 2026
#pragma once
#include <cstdint>

namespace sc::ast {
/// @brief Which accessors a class variable, instance variable, or constant declares, e.g., `<`, `>`, and `<>`.
///
/// The values are bit flags, so they can be combined and tested with has().
enum struct ReadWriteAccessor : std::uint8_t {
    /// Declares no accessor.
    Private = 0b000,
    /// `<`, generates a getter.
    ExternallyReadable = 0b001,
    /// `>`, generates a setter.
    ExternallyWriteable = 0b010,
    /// `<>`, generates a getter and a setter.
    ExternallyReadAndWritable = 0b011,
    /// A constant.
    Const = 0b100,
};

/// @brief Whether any of the bits of @p flag are set in @p r.
[[nodiscard]] constexpr bool has(ReadWriteAccessor r, ReadWriteAccessor flag) {
    return static_cast<std::uint8_t>(r) & static_cast<std::uint8_t>(flag);
}

/// @brief @p from without the bits of @p to_remove.
[[nodiscard]] constexpr ReadWriteAccessor remove(ReadWriteAccessor from, ReadWriteAccessor to_remove) {
    return static_cast<ReadWriteAccessor>(static_cast<std::uint8_t>(from) & ~static_cast<std::uint8_t>(to_remove));
}

/// @brief The union of all the flags in @p Rs.
template <ReadWriteAccessor... Rs> [[nodiscard]] constexpr ReadWriteAccessor combine() {
    return static_cast<ReadWriteAccessor>((static_cast<std::uint8_t>(Rs) | ...));
}
}
