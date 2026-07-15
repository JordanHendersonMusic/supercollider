#pragma once
#include <memory>
#include <optional>
#include <vector>
#include "text_info.hpp"
#include "text_location.hpp"
#include "tokens.hpp"

namespace sc::parser {

class ErrorHandler {
public:
    // Called during lexing error
    virtual void operator()(std::shared_ptr<const sc::parser::TextInfo>, sc::lex::TokenType, sc::lex::SourceCodeRange,
                            std::optional<sc::lex::SourceCodeRange>) = 0;

    // Called from memory error, unlikely.
    virtual void operator()(std::shared_ptr<const sc::parser::TextInfo>, sc::lex::SourceCodeRange,
                            const std::string&) = 0;

    // Called for parsing error.
    // Note, the 'int' type here is a parser::symbol_kind_type. The parser has good conversion functions.
    virtual void operator()(std::shared_ptr<const sc::parser::TextInfo>, std::vector<const char*> expected,
                            sc::lex::SourceCodeRange got_location, const char* got_name) = 0;

    virtual ~ErrorHandler() = default;
};


}
