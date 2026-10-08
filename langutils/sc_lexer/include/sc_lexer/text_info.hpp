// Copyright Jordan Henderson 2026
#pragma once

#include "codepoint_stream.hpp"
#include "normalise_source.hpp"

namespace sc::lex {

struct TextInfo {
    NormalisedSource source;
    FileCodeLocation source_start_in_file;
    const char* file_path; // can be nullptr;
    bool is_class_file;

    [[nodiscard]] std::string_view read(SourceCodeRange r) const noexcept {
        const char* str = source.as_string().c_str();
        return { str + r.begin.absolute, r.size() };
    }

    [[nodiscard]] CodePointStream code_point_stream(SourceCodeRange scr) const noexcept {
        return { source, source_start_in_file, scr };
    }
    [[nodiscard]] CodePointStream code_point_stream(SourceCodeLocation start) const noexcept {
        return { source, source_start_in_file, start };
    }
    [[nodiscard]] CodePointStream code_point_stream() const noexcept {
        return { source, source_start_in_file };
    }
};

}
