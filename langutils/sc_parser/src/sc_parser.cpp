#include "sc_parser.hpp"
#include "codepoint_stream.hpp"


#include "parser_context.hpp"
#include <memory>

#include "sc_grammar_parser.hpp"

namespace sc::parser {

[[nodiscard]] std::tuple<graph::ParserGraph, bool>
parse(std::shared_ptr<const TextInfo> text_info, ::sc::lex::CodePointStream cps, std::shared_ptr<ErrorHandler> err) {
    ParserContext cxt {
        text_info, cps,
        Action {}, text_info->is_class_file ? ParserContext::Mode::ClassLibrary : ParserContext::Mode::CommandInitial,
        err,
    };

    parser p { cxt };

    const auto ret = p();
    return { std::move(cxt.graph), ret };
}

std::tuple<graph::ParserGraph, bool> parse(std::shared_ptr<const TextInfo> text_info, sc::lex::SourceCodeRange r,
                                           std::shared_ptr<ErrorHandler> err) {
    return parse(text_info, text_info->code_point_stream(r), err);
};

std::tuple<graph::ParserGraph, bool> parse(std::shared_ptr<const TextInfo> text_info, sc::lex::SourceCodeLocation r,
                                           std::shared_ptr<ErrorHandler> err) {
    return parse(text_info, text_info->code_point_stream(r), err);
};

std::tuple<graph::ParserGraph, bool> parse(std::shared_ptr<const TextInfo> text_info,
                                           std::shared_ptr<ErrorHandler> err) {
    return parse(text_info, text_info->code_point_stream(), err);
}


}
