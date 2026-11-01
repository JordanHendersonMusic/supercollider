#pragma once

/// @file
/// @brief Positions as the tools hand them out: lines and codepoint columns, file locations, and URIs.

#include "sc_source/text_location.hpp"

#include <cstdint>
#include <filesystem>
#include <string>
#include <string_view>
#include <vector>

namespace sc::query {

////////////////////////////////////////////////////////////////////////////////
// API
////////////////////////////////////////////////////////////////////////////////

/// @brief A place in a text: a zero based line, and a zero based column counted in codepoints, as the lexer counts.
struct Position {
    std::uint32_t line { 0 };
    std::uint32_t column { 0 };
    [[nodiscard]] bool operator<(const Position& o) const {
        return line < o.line || (line == o.line && column < o.column);
    }
    [[nodiscard]] bool operator<=(const Position& o) const { return !(o < *this); }
    [[nodiscard]] bool operator==(const Position& o) const { return line == o.line && column == o.column; }
};

/// @brief A range of a text, its end exclusive.
struct Range {
    Position begin {};
    Position end {};
    /// @brief Whether @p p is inside, counting the end, so a cursor just after a word is on it.
    [[nodiscard]] bool touches(Position p) const { return begin <= p && p <= end; }
    /// @brief Whether this is no larger than @p o, by lines then columns.
    [[nodiscard]] bool smaller_than(const Range& o) const;
};

/// @brief The range of a source range.
[[nodiscard]] Range to_range(const src::SourceCodeRange& r);

/// @brief A range in a file. The path is empty when the text isn't in a file.
struct Location {
    std::filesystem::path path {};
    Range range {};
};

/// @brief The lines of a text, to turn columns between codepoints, UTF-16 units and byte offsets.
///
/// Lines end at `\n`, `\r\n`, `\r`, `\v` and `\f`, as the normaliser treats them all as line breaks.
class LineIndex {
public:
    LineIndex() = default;
    explicit LineIndex(std::string_view text);

    /// @brief The codepoint column of a UTF-16 column on a line.
    [[nodiscard]] std::uint32_t utf16_to_codepoint(std::uint32_t line, std::uint32_t utf16_column) const;
    /// @brief The UTF-16 column of a codepoint column on a line.
    [[nodiscard]] std::uint32_t codepoint_to_utf16(std::uint32_t line, std::uint32_t codepoint_column) const;
    /// @brief The byte offset of a position, clamped to the line's end.
    [[nodiscard]] std::size_t offset(Position p) const;
    /// @brief The position of a byte offset.
    [[nodiscard]] Position position(std::size_t offset) const;
    /// @brief The text of a line, without its line break.
    [[nodiscard]] std::string_view line_text(std::uint32_t line) const;
    [[nodiscard]] std::uint32_t num_lines() const { return static_cast<std::uint32_t>(m_starts.size()); }

private:
    std::string_view m_text {};
    std::vector<std::size_t> m_starts {};
    std::vector<std::size_t> m_ends {};
};

/// @brief The path of a `file:` URI, or empty if it isn't one.
[[nodiscard]] std::filesystem::path uri_to_path(std::string_view uri);
/// @brief The `file:` URI of a path.
[[nodiscard]] std::string path_to_uri(const std::filesystem::path& path);
/// @brief A path made absolute and normal, to compare paths with.
[[nodiscard]] std::filesystem::path normal_path(const std::filesystem::path& path);

/// @brief The number of UTF-8 codepoints in @p s.
[[nodiscard]] std::uint32_t count_codepoints(std::string_view s);

}
