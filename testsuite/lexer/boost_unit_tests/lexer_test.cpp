#include <boost/test/unit_test.hpp>
#include "lexer.hpp"

using namespace sc::lex;
using namespace sc::lex::literals;

static constexpr std::array default_gobble {
    TokenType::Interpret,        TokenType::Space,
    TokenType::NewLine,          TokenType::Tab,
    TokenType::Comment,          TokenType::DocumentationComment,
    TokenType::MultiLineComment,
};

template <size_t N, size_t M>
void match(const char* text, const std::array<TokenType, N>& to_find, const std::array<TokenType, M>& to_gobble) {
    const auto text_len = strlen(text);

    CodePointStream stream { false, text, text_len, {} };

    TypeAndLocationAction action {};

    for (const TokenType t : to_find) {
        auto o = lexer(stream, action);

        while (std::find(to_gobble.begin(), to_gobble.end(), o.type) != to_gobble.end()) {
            o = lexer(stream, action);
        }

        BOOST_TEST(o.type == t);
    }

    auto o = lexer(stream, action);
    while (std::find(to_gobble.begin(), to_gobble.end(), o.type) != to_gobble.end() && o.type != TokenType::EndOfFile) {
        o = lexer(stream, action);
    }

    BOOST_TEST(o.type == TokenType::EndOfFile);
}

BOOST_AUTO_TEST_CASE(basic) {
    const char* text = "   some 0.312 \\hello \n 'text';  \n"
                       "\n"
                       "\n"
                       "-0.2 \t  _Pri Foo _T:\\6( ";

    // NO gobble
    match(text,
          std::array {
              TokenType::Interpret,   TokenType::Space,     TokenType::Name,        TokenType::Space,
              TokenType::Float,       TokenType::Space,     TokenType::SymbolSlash, TokenType::Space,
              TokenType::NewLine,     TokenType::Space,     TokenType::SymbolQuote, ';'_tokentype,
              TokenType::Space,       TokenType::NewLine,   '-'_tokentype,          TokenType::Float,
              TokenType::Space,       TokenType::Tab,       TokenType::Space,       TokenType::PrimitiveName,
              TokenType::Space,       TokenType::ClassName, TokenType::Space,       TokenType::KeywordBinaryOperator,
              TokenType::SymbolSlash, '('_tokentype,        TokenType::Space,       TokenType::EndOfFile,
          },
          std::array<TokenType, 0> {});

    match("    *new ", std::array { '*'_tokentype, TokenType::Name }, default_gobble);
    match("    const nl = \"\\n\"; \n\t*new ",
          std::array {
              TokenType::Const,
              TokenType::Name,
              '='_tokentype,
              TokenType::StringLine,
              ';'_tokentype,
              '*'_tokentype,
              TokenType::Name,
          },
          default_gobble);
}

BOOST_AUTO_TEST_CASE(fn) {
    const char* text = "       \t\t\t\t\t\t        var f = {|a,b,c|\na+b + c}\n\n\t;   ";

    match(text,
          std::array {
              TokenType::Var,
              TokenType::Name,
              '='_tokentype,
              '{'_tokentype,
              '|'_tokentype,
              TokenType::Name,
              ','_tokentype,
              TokenType::Name,
              ','_tokentype,
              TokenType::Name,
              '|'_tokentype,
              TokenType::Name,
              '+'_tokentype,
              TokenType::Name,
              '+'_tokentype,
              TokenType::Name,
              '}'_tokentype,
              ';'_tokentype,
          },
          default_gobble);
}

BOOST_AUTO_TEST_CASE(strings) {
    match(R"%(   "(\""   )%", std::array { TokenType::StringLine }, default_gobble);
    match(R"%( "(\"" )%", std::array { TokenType::StringLine }, default_gobble);
    match(R"%( "\")" abs )%", std::array { TokenType::StringLine, TokenType::Name }, default_gobble);
    match(R"%( "◎" bang )%", std::array { TokenType::StringLine, TokenType::Name }, default_gobble);
    match(R"%( 
			"The function % should behave the same for a PatternProxy and its source:\n%\n"
    )%",
          std::array { TokenType::StringLine }, default_gobble);
}

BOOST_AUTO_TEST_CASE(symbol) { match("\\)", std::array { TokenType::SymbolSlash, ')'_tokentype }, default_gobble); }

BOOST_AUTO_TEST_CASE(ascii) {
    match("$a", std::array { TokenType::Ascii }, default_gobble);
    match("$a)", std::array { TokenType::Ascii, ')'_tokentype }, default_gobble);
    match("$\\n", std::array { TokenType::Ascii }, default_gobble);
    match("$\\n)", std::array { TokenType::Ascii, ')'_tokentype }, default_gobble);
    match("$ ", std::array { TokenType::Ascii }, default_gobble);
    match("$    bang  ", std::array { TokenType::Ascii, TokenType::Name }, default_gobble);
}

BOOST_AUTO_TEST_CASE(larger_obj) {
    const auto txt = R"%%(
Object {
	classvar <dependantsDictionary, currentEnvironment, topEnvironment, <uniqueMethods;

	const nl = "\n";

	*new { arg maxSize = 0; _BasicNew
    )%%";
    using T = TokenType;
    // clang-format off
    match(txt,
          std::array {
              T::ClassName,  '{'_tokentype, 
              T::ClassVar,
              '<'_tokentype, T::Name, ','_tokentype,
              T::Name, ','_tokentype,
              T::Name,','_tokentype, 
              '<'_tokentype, T::Name,';'_tokentype, 
              T::Const, T::Name, '='_tokentype, T::StringLine, ';'_tokentype,
              '*'_tokentype, T::Name, '{'_tokentype,
              T::Arg, T::Name, '='_tokentype, T::Integer, ';'_tokentype,
              T::PrimitiveName
          },
          default_gobble);
    // clang-format on
}

BOOST_AUTO_TEST_CASE(recorder) {
    const auto txt = R"%%(
Recorder {

	var <server, <>numChannels;
	var >recHeaderFormat, >recSampleFormat, >recBufSize;
	var recordBuf, recordNode, synthDef;
    )%%";
    using T = TokenType;
    // clang-format off
    match(txt,
          std::array {
              T::ClassName,  '{'_tokentype, 
              T::Var,
              '<'_tokentype, T::Name, ','_tokentype,
              T::ReadWriteVar, T::Name, ';'_tokentype,

              T::Var,
              '>'_tokentype, T::Name, ','_tokentype,
              '>'_tokentype, T::Name, ','_tokentype,
              '>'_tokentype, T::Name, ';'_tokentype,

              T::Var,
              T::Name, ','_tokentype,
              T::Name, ','_tokentype,
              T::Name, ';'_tokentype,
          },
          default_gobble);
    // clang-format on
}
