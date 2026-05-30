#include <text_location.hpp>

using namespace sc::lex;

// assuming the line number and column are correct and that the source text is the same means we only need to comare
// the absolute byte offset.
[[nodiscard]] bool SourceCodeLocation::operator==(const SourceCodeLocation& o) const noexcept {
    return absolute == o.absolute && lineNumber == o.lineNumber && column == o.column;
}

[[nodiscard]] bool SourceCodeLocation::operator!=(const SourceCodeLocation& o) const noexcept { return !(*this == o); }

[[nodiscard]] bool SourceCodeLocation::operator<(const SourceCodeLocation& o) const noexcept {
    return absolute < o.absolute;
}

[[nodiscard]] bool SourceCodeLocation::operator>(const SourceCodeLocation& o) const noexcept {
    return absolute > o.absolute;
}

[[nodiscard]] bool SourceCodeLocation::operator<=(const SourceCodeLocation& o) const noexcept {
    return absolute <= o.absolute;
}

[[nodiscard]] bool SourceCodeLocation::operator>=(const SourceCodeLocation& o) const noexcept {
    return absolute >= o.absolute;
}

[[nodiscard]] bool FileCodeLocation::operator==(const FileCodeLocation& o) const noexcept {
    return absolute == o.absolute && lineNumber == o.lineNumber && column == o.column;
}

[[nodiscard]] bool FileCodeLocation::operator!=(const FileCodeLocation& o) const noexcept { return !(*this == o); }

[[nodiscard]] bool FileCodeLocation::operator<(const FileCodeLocation& o) const noexcept {
    return absolute < o.absolute;
}

[[nodiscard]] bool FileCodeLocation::operator>(const FileCodeLocation& o) const noexcept {
    return absolute > o.absolute;
}

[[nodiscard]] bool FileCodeLocation::operator<=(const FileCodeLocation& o) const noexcept {
    return absolute <= o.absolute;
}

[[nodiscard]] bool FileCodeLocation::operator>=(const FileCodeLocation& o) const noexcept {
    return absolute >= o.absolute;
}

[[nodiscard]] SourceCodeRange SourceCodeRange::range(SourceCodeRange left, SourceCodeRange right) {
    return { left.begin, right.end };
}

[[nodiscard]] std::size_t sc::lex::SourceCodeRange::size() const { return end.absolute - begin.absolute; }

[[nodiscard]] std::size_t sc::lex::SourceCodeRange::line_count() const {
    return (end.lineNumber - begin.lineNumber) + 1;
}

[[nodiscard]] bool sc::lex::SourceCodeRange::operator==(const SourceCodeRange& o) const noexcept {
    return begin == o.begin && end == o.end;
}
[[nodiscard]] bool sc::lex::SourceCodeRange::operator!=(const SourceCodeRange& o) const noexcept {
    return !(*this == o);
}
[[nodiscard]] bool sc::lex::SourceCodeRange::operator<(const SourceCodeRange& o) const noexcept {
    return begin < o.begin && end < o.begin;
}
[[nodiscard]] bool sc::lex::SourceCodeRange::operator>(const SourceCodeRange& o) const noexcept {
    return begin < o.end && end > o.end;
}

[[nodiscard]] sc::lex::FileCodeRange sc::lex::FileCodeRange::range(FileCodeRange left, FileCodeRange right) {
    return { left.begin, right.end };
}

[[nodiscard]] std::size_t sc::lex::FileCodeRange::size() const { return end.absolute - begin.absolute; }

[[nodiscard]] std::size_t sc::lex::FileCodeRange::line_count() const { return (end.lineNumber - begin.lineNumber) + 1; }

[[nodiscard]] bool sc::lex::FileCodeRange::operator==(const FileCodeRange& o) const noexcept {
    return begin == o.begin && end == o.end;
}
[[nodiscard]] bool sc::lex::FileCodeRange::operator!=(const FileCodeRange& o) const noexcept { return !(*this == o); }
[[nodiscard]] bool sc::lex::FileCodeRange::operator<(const FileCodeRange& o) const noexcept {
    return begin < o.begin && end < o.begin;
}
[[nodiscard]] bool sc::lex::FileCodeRange::operator>(const FileCodeRange& o) const noexcept {
    return begin < o.end && end > o.end;
}
