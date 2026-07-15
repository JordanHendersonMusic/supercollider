// Copyright Jordan Henderson 2026
#include "sc_parser.hpp"
#include "codepoint_stream.hpp"


#include "parser_context.hpp"
#include <memory>

#include "sc_grammar_parser.hpp"

namespace sc::parser {

[[nodiscard]] std::tuple<graph::NodeGraph, bool> parse(std::shared_ptr<const TextInfo> text_info,
                                                       ::sc::lex::CodePointStream cps) {
    ParserContext cxt {
        text_info,
        cps,
        Action {},
        text_info->is_class_file ? ParserContext::Mode::ClassLibrary : ParserContext::Mode::CommandInitial,
    };

    parser p { cxt };

    const auto ret = p(); // if ret is false, some unknown error was encountered
    const bool result = ret && cxt.graph;
    return { std::move(cxt.graph), result };
}

std::tuple<graph::NodeGraph, bool> parse(std::shared_ptr<const TextInfo> text_info, sc::lex::SourceCodeRange r) {
    return parse(text_info, text_info->code_point_stream(r));
};

std::tuple<graph::NodeGraph, bool> parse(std::shared_ptr<const TextInfo> text_info, sc::lex::SourceCodeLocation r) {
    return parse(text_info, text_info->code_point_stream(r));
};

std::tuple<graph::NodeGraph, bool> parse(std::shared_ptr<const TextInfo> text_info) {
    return parse(text_info, text_info->code_point_stream());
}
}
