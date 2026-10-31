// Copyright Jordan Henderson 2026
#pragma once
#include <array>
#include <charconv>
#include <cstdint>
#include <optional>
#include <string_view>
#include <system_error>
#include <utility>

namespace sc::ast {
/// @brief The rule set a piece of code is compiled with. `Legacy` is the behaviour before any versioned change.
enum struct LanguageVersion : std::uint8_t { Legacy, V3_16 };

/// @brief A SuperCollider release that changed the language rules.
struct LanguageRelease {
    /// The major number of the release.
    unsigned major;
    /// The minor number of the release.
    unsigned minor;
    /// The rules from this release onwards.
    LanguageVersion version;
};

/// The releases that changed the rules, oldest first. Those before the first are Legacy.
inline constexpr std::array language_releases { LanguageRelease { 3, 16, LanguageVersion::V3_16 } };

/// @brief The version a release compiles as: the newest version at or before it.
/// @return The version, or nothing after the latest release, as its rules aren't known.
[[nodiscard]] constexpr std::optional<LanguageVersion> version_for_release(unsigned major, unsigned minor) {
    const auto& latest = language_releases.back();
    if (std::pair { major, minor } > std::pair { latest.major, latest.minor })
        return std::nullopt;
    auto out = LanguageVersion::Legacy;
    for (const auto& r : language_releases)
        if (std::pair { major, minor } >= std::pair { r.major, r.minor })
            out = r.version;
    return out;
}

/// @brief Parses a release, e.g., `3.16`, as the version it compiles as, or `legacy`.
/// @param release The text to parse.
/// @return The version, or nothing if @p release isn't `major.minor` or `legacy`, or is after the latest release.
[[nodiscard]] inline std::optional<LanguageVersion> parse_release(std::string_view release) {
    if (release == "legacy")
        return LanguageVersion::Legacy;
    unsigned major { 0 };
    unsigned minor { 0 };
    const auto* end = release.data() + release.size();
    const auto [dot, major_error] = std::from_chars(release.data(), end, major);
    if (major_error != std::errc {} || dot == end || *dot != '.')
        return std::nullopt;
    const auto [last, minor_error] = std::from_chars(dot + 1, end, minor);
    if (minor_error != std::errc {} || last != end)
        return std::nullopt;
    return version_for_release(major, minor);
}

/// @brief The version as it is written in a release, e.g., `3.16`, or `legacy`.
[[nodiscard]] constexpr const char* to_string(LanguageVersion v) {
    switch (v) {
    case LanguageVersion::Legacy:
        return "legacy";
    case LanguageVersion::V3_16:
        return "3.16";
    }
    return "unknown";
}

}
