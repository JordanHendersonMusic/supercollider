// Copyright Jordan Henderson 2026
#pragma once

#include "codepoint_stream.hpp"
#include "normalise_source.hpp"
#include "text_location.hpp"

namespace sc::parser {

struct TextInfo {
    sc::lex::NormalisedSource source;
    sc::lex::FileCodeLocation source_start_in_file;
    const char* file_path; // can be nullptr;
    bool is_class_file;

    [[nodiscard]] std::tuple<const char*, std::size_t> read(sc::lex::SourceCodeRange r) const noexcept {
        const char* str = source.as_string().c_str();
        return { str + r.begin.absolute, r.size() };
    }

    [[nodiscard]] sc::lex::CodePointStream code_point_stream(sc::lex::SourceCodeRange scr) const noexcept {
        return { source, source_start_in_file, scr };
    }
    [[nodiscard]] sc::lex::CodePointStream code_point_stream(sc::lex::SourceCodeLocation start) const noexcept {
        return { source, source_start_in_file, start };
    }
    [[nodiscard]] sc::lex::CodePointStream code_point_stream() const noexcept {
        return { source, source_start_in_file };
    }
};

}
