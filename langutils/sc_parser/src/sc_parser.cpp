// Copyright Jordan Henderson 2026
#include "sc_parser/sc_parser.hpp"
#include "sc_lexer/details/codepoint_stream.hpp"


#include "parser_context.hpp"
#include <memory>

#include "sc_grammar_impl.hpp"
#include "sc_grammar_parser.hpp"
#include "sc_source/text_info.hpp"
#include "sc_source/text_location.hpp"
#include "sc_parser/ast.hpp"

namespace sc::ast {

namespace {
parser::ParserContext::Mode mode_of(const src::TextInfo& text_info) {
    using Mode = parser::ParserContext::Mode;
    switch (text_info.kind) {
    case src::TextKind::ClassFile:
        return Mode::ClassLibrary;
    case src::TextKind::ScdFile:
        return Mode::CommandInitial;
    case src::TextKind::InterpretedFunction:
        return Mode::FunctionInitial;
    }
    return Mode::CommandInitial;
}
}

[[nodiscard]] std::tuple<std::shared_ptr<ASTGraph>, bool> parse(std::shared_ptr<const src::TextInfo> text_info,
                                                                ::sc::lex::CodePointStream cps) {
    parser::ParserContext ctx { text_info, mode_of(*text_info) };
    parser::tokenise(ctx, std::move(cps));
    ctx.claim_file_version();

    parser::parser p { ctx };
    ctx.owner = &p;

    // Bison's result, 0 if it accepted the input, otherwise it aborted or ran out of memory.
    const auto ret = p();
    // The parse can end on an error without recovering from it.
    parser::error_recovery::report(ctx);
    ctx.report_misplaced_versions();
    const bool accepted = ret == 0 && ctx.graph;
    return { std::move(ctx.graph), accepted };
}

std::optional<std::vector<sc::lex::TokenType>> expected_tokens(std::shared_ptr<const src::TextInfo> text_info,
                                                               sc::src::SourceCodeRange r) {
    parser::ParserContext ctx { text_info, mode_of(*text_info) };
    parser::tokenise(ctx, { text_info->source, text_info->source_start_in_file, r });
    ctx.claim_file_version();

    // Before the end of file, and the brackets inserted to close those left open.
    auto stop = ctx.tokens.next_index();
    while (stop != parser::TokenIndex { 0 }) {
        const auto& t = ctx.tokens[sc::util::retreat(stop)];
        if (t.token != sc::lex::TokenType::EndOfFile && !t.inserted())
            break;
        stop = sc::util::retreat(stop);
    }
    ctx.stop_at = stop;

    parser::parser p { ctx };
    ctx.owner = &p;
    try {
        (void)p();
    } catch (const parser::ParserContext::Stopped&) {}
    if (ctx.recovered_to_stop)
        return std::nullopt;
    return std::move(ctx.expected_at_stop);
}

std::tuple<std::shared_ptr<ASTGraph>, bool> parse(std::shared_ptr<const src::TextInfo> text_info,
                                                  sc::src::SourceCodeRange r) {
    return parse(text_info, sc::lex::CodePointStream { text_info->source, text_info->source_start_in_file, r });
};

std::tuple<std::shared_ptr<ASTGraph>, bool> parse(std::shared_ptr<const src::TextInfo> text_info,
                                                  sc::src::SourceCodeLocation r) {
    return parse(text_info, { text_info->source, text_info->source_start_in_file, r });
};

std::tuple<std::shared_ptr<ASTGraph>, bool> parse(std::shared_ptr<const sc::src::TextInfo> text_info) {
    return parse(text_info, { text_info->source, text_info->source_start_in_file });
}
}
