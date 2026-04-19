#pragma once

#include "lexer.hpp"

#include <optional>


struct CompilerState {
    enum struct Result { NotStarted, InProgress, Failed, Ok };
    enum struct Mode { CommandLine, ClassLibrary };

    CompilerState() = delete;
    CompilerState(CompilerState&&) noexcept = default;
    CompilerState& operator=(CompilerState&&) noexcept = default;
    CompilerState(const CompilerState&) = delete;
    CompilerState& operator=(const CompilerState&) = delete;
    // CompilerState()

    Mode mode;

    struct PyrSymbol* file_name; // or nullptr

    Result lexing_result { Result::NotStarted };
    Result parsing_result { Result::NotStarted };
    Result compiling_result { Result::NotStarted };

    sc::lex::CodePointStream code_point_stream;
};


// We are limited to having only one compiler at a time, this is a limit of bison. This could change in future.
extern std::optional<CompilerState> gCompilerState;
