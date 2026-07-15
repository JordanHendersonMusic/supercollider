#include "sc_grammar_impl.hpp"
#include "codepoint_stream.hpp"
#include "lexer.hpp"
#include "node_graph_diagnostic.hpp"
#include "parser_context.hpp"
#include "text_location.hpp"

void sc::parser::parser::report_syntax_error(const context& symbol_cxt) const {
    std::vector<symbol_kind_type> expected(12);
    expected.resize(symbol_cxt.expected_tokens(expected.data(), 12));

    std::vector<int> expected_int(expected.size());
    std::transform(expected.begin(), expected.end(), expected_int.begin(),
                   [](symbol_kind_type r) -> int { return static_cast<int>(r); });

    cxt.unexpected_token_error = UnexpectedToken { symbol_cxt.location(), std::move(expected_int),
                                                   static_cast<int>(symbol_cxt.lookahead().kind_) };
}

void sc::parser::parser::error(const sc::lex::SourceCodeRange& loc, const std::string& message) {
    // TODO: temporary code.
    ////////////////	cxt.error_handler->operator()(cxt.text_info, loc, message);
}


namespace sc::parser::error_recovery {

void region_separator(ParserContext& cxt, sc ::lex::SourceCodeRange last_valid) {
    auto unexpected = cxt.consume_error();
    std::string msg { "Insert ';' after this expression to separate it from the following." };

    const auto [highlight_ptr, highlight_sz] = cxt.text_info->read(last_valid);
    msg += " Recommendation: '";
    msg.append(highlight_ptr, highlight_sz);
    msg += ";'.";
    cxt.graph.add_diagnostic({ "Missing semicolon between regions.",
                               { cxt.text_info, last_valid.end, last_valid.end },
                               graph::Diagnostic::Severity::Warning,
                               msg });
}

void expr(ParserContext& cxt) {
    using Symbol = sc::parser::parser::symbol_kind_type;
    auto unexpected = *cxt.consume_error();
    const auto got = static_cast<Symbol>(unexpected.got);
    const sc::lex::SourceCodeRange& loc = unexpected.location;

    const char* got_name = sc::parser::parser::symbol_name(got);

    std::string msg = "Expected: ";
    const auto sz = unexpected.expected.size();
    for (size_t i { 0 }; i < sz; ++i) {
        msg += sc::parser::parser::symbol_name(static_cast<Symbol>(unexpected.expected[i]));

        if (i + 2 == sz)
            msg += ", or ";
        else if (i + 2 < sz)
            msg += ", ";
    }

    msg += " but received ";
    msg += got_name;

    const auto [highlight_ptr, highlight_sz] = cxt.text_info->read(unexpected.location);
    msg.append(highlight_ptr, highlight_sz);

    cxt.graph.add_diagnostic(
        { "Unexpected token.", { cxt.text_info, unexpected.location }, graph::Diagnostic::Severity::Error, msg, {} });
}

}
