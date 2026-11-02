#include "sc_query/text.hpp"

#include <algorithm>
#include <cstdio>
#include <system_error>

namespace sc::query {

namespace {
bool is_continuation(unsigned char c) { return (c & 0xC0) == 0x80; }

// The bytes of the codepoint starting at @p s[i].
std::size_t codepoint_length(std::string_view s, std::size_t i) {
    const auto c = static_cast<unsigned char>(s[i]);
    std::size_t n = 1;
    if (c >= 0xF0)
        n = 4;
    else if (c >= 0xE0)
        n = 3;
    else if (c >= 0xC0)
        n = 2;
    std::size_t k = 1;
    while (k < n && i + k < s.size() && is_continuation(static_cast<unsigned char>(s[i + k])))
        ++k;
    return k;
}
}

bool Range::smaller_than(const Range& o) const {
    const auto lines = end.line - begin.line;
    const auto other_lines = o.end.line - o.begin.line;
    if (lines != other_lines)
        return lines < other_lines;
    const auto columns = static_cast<std::int64_t>(end.column) - begin.column;
    const auto other_columns = static_cast<std::int64_t>(o.end.column) - o.begin.column;
    return columns <= other_columns;
}

Range to_range(const src::SourceCodeRange& r) {
    return { { r.begin.line_number, r.begin.codepoint_in_line }, { r.end.line_number, r.end.codepoint_in_line } };
}

LineIndex::LineIndex(std::string_view text): m_text(text) {
    std::size_t start = 0;
    for (std::size_t i = 0; i < text.size(); ++i) {
        const char c = text[i];
        m_has_surrogate_pairs = m_has_surrogate_pairs || static_cast<unsigned char>(c) >= 0xF0;
        if (c == '\n' || c == '\r' || c == '\v' || c == '\f') {
            m_starts.push_back(start);
            m_ends.push_back(i);
            if (c == '\r' && i + 1 < text.size() && text[i + 1] == '\n')
                ++i;
            start = i + 1;
        }
    }
    m_starts.push_back(start);
    m_ends.push_back(text.size());
}

std::string_view LineIndex::line_text(std::uint32_t line) const {
    if (line >= m_starts.size())
        return {};
    return m_text.substr(m_starts[line], m_ends[line] - m_starts[line]);
}

std::uint32_t LineIndex::utf16_to_codepoint(std::uint32_t line, std::uint32_t utf16_column) const {
    if (!m_has_surrogate_pairs)
        return utf16_column;
    const auto text = line_text(line);
    std::uint32_t units = 0;
    std::uint32_t codepoints = 0;
    for (std::size_t i = 0; i < text.size() && units < utf16_column;) {
        const auto n = codepoint_length(text, i);
        units += n == 4 ? 2 : 1;
        ++codepoints;
        i += n;
    }
    if (units < utf16_column)
        codepoints += utf16_column - units;
    return codepoints;
}

std::uint32_t LineIndex::codepoint_to_utf16(std::uint32_t line, std::uint32_t codepoint_column) const {
    if (!m_has_surrogate_pairs)
        return codepoint_column;
    const auto text = line_text(line);
    std::uint32_t units = 0;
    std::uint32_t codepoints = 0;
    std::size_t i = 0;
    for (; i < text.size() && codepoints < codepoint_column; ++codepoints) {
        const auto n = codepoint_length(text, i);
        units += n == 4 ? 2 : 1;
        i += n;
    }
    return units + (codepoint_column - codepoints);
}

std::size_t LineIndex::offset(Position p) const {
    if (m_starts.empty())
        return 0;
    if (p.line >= m_starts.size())
        return m_text.size();
    const auto text = line_text(p.line);
    std::size_t i = 0;
    for (std::uint32_t c = 0; c < p.column && i < text.size(); ++c)
        i += codepoint_length(text, i);
    return m_starts[p.line] + i;
}

Position LineIndex::position(std::size_t offset) const {
    if (m_starts.empty())
        return {};
    const auto it = std::upper_bound(m_starts.begin(), m_starts.end(), offset);
    const auto line = static_cast<std::uint32_t>(std::max<std::ptrdiff_t>(0, (it - m_starts.begin()) - 1));
    const auto start = m_starts[line];
    const auto end = std::min(offset, m_ends[line]);
    return { line, count_codepoints(m_text.substr(start, end - start)) };
}

std::uint32_t count_codepoints(std::string_view s) {
    std::uint32_t n = 0;
    for (const char c : s)
        if (!is_continuation(static_cast<unsigned char>(c)))
            ++n;
    return n;
}

std::filesystem::path uri_to_path(std::string_view uri) {
    constexpr std::string_view scheme = "file://";
    if (uri.substr(0, scheme.size()) != scheme)
        return {};
    uri.remove_prefix(scheme.size());
    // A host, e.g., file://localhost/a, is skipped.
    if (!uri.empty() && uri.front() != '/') {
        const auto slash = uri.find('/');
        if (slash == std::string_view::npos)
            return {};
        uri.remove_prefix(slash);
    }
    std::string decoded;
    for (std::size_t i = 0; i < uri.size(); ++i) {
        if (uri[i] == '%' && i + 2 < uri.size()) {
            const std::string hex { uri.substr(i + 1, 2) };
            decoded += static_cast<char>(std::strtol(hex.c_str(), nullptr, 16));
            i += 2;
        } else {
            decoded += uri[i];
        }
    }
#ifdef _WIN32
    // file:///C:/a is /C:/a here.
    if (decoded.size() > 2 && decoded[0] == '/' && decoded[2] == ':')
        decoded.erase(0, 1);
#endif
    return std::filesystem::u8path(decoded);
}

std::string path_to_uri(const std::filesystem::path& path) {
    auto text = path.generic_u8string();
    std::string out = "file://";
    if (text.empty() || text.front() != '/')
        out += '/';
    for (const char c : text) {
        const auto u = static_cast<unsigned char>(c);
        if ((u >= 'a' && u <= 'z') || (u >= 'A' && u <= 'Z') || (u >= '0' && u <= '9') || c == '/' || c == '-'
            || c == '_' || c == '.' || c == '~' || (c == ':' && out.size() <= 10)) {
            out += c;
        } else {
            char escaped[4];
            std::snprintf(escaped, sizeof escaped, "%%%02X", static_cast<unsigned>(u));
            out += escaped;
        }
    }
    return out;
}

std::filesystem::path normal_path(const std::filesystem::path& path) {
    if (path.empty())
        return path;
    std::error_code ec;
    auto absolute = std::filesystem::absolute(path, ec);
    if (ec)
        absolute = path;
    auto canonical = std::filesystem::weakly_canonical(absolute, ec);
    return ec ? absolute.lexically_normal() : canonical;
}

}
