#define BOOST_TEST_MODULE sc_lexer_tests
#include <boost/test/included/unit_test.hpp>
#include "codepoint.hpp"
#include <cstddef>
#include <source_utils.hpp>
#include <lexer.hpp>

using namespace sc::lex;

BOOST_TEST_DONT_PRINT_LOG_VALUE(TokenType);

BOOST_AUTO_TEST_CASE(codepointer_iterator_forward) {
    using CPI = utils::CodePointIterator;
    const char* const source = "Here is some text.\n∃\t∑\n.";
    auto m_it = CPI::make(source, source + strlen(source) + 1);
    BOOST_TEST(m_it.has_value());
    auto it = std::move(*m_it);

    const std::array<sc::lex::CodePoint, 25> expected {
        'H', 'e', 'r', 'e', ' ', 'i',  's',    ' ',  's',    'o',  'm', 'e', ' ',
        't', 'e', 'x', 't', '.', '\n', 0x2203, '\t', 0x2211, '\n', '.', 0,
    };

    std::size_t i = 0;
    for (auto r = it.forwards(); r; r = it.forwards()) {
        BOOST_TEST(expected[i] == *r);
        ++i;
    }

    BOOST_TEST(i == expected.size());
}

BOOST_AUTO_TEST_CASE(codepointer_iterator_backward) {
    using CPI = utils::CodePointIterator;
    const char* const source = "Here is some text.\n∃\t∑\n.";
    auto m_it = CPI::make(source, source + strlen(source) + 1, source + strlen(source) + 1);
    BOOST_TEST(m_it.has_value());
    auto it = std::move(*m_it);

    const std::array<sc::lex::CodePoint, 25> expected {
        'H', 'e', 'r', 'e', ' ', 'i',  's',    ' ',  's',    'o',  'm', 'e', ' ',
        't', 'e', 'x', 't', '.', '\n', 0x2203, '\t', 0x2211, '\n', '.', 0,
    };

    std::size_t i = 0;
    for (auto r = it.backwards(); r; r = it.backwards()) {
        BOOST_TEST(expected[24 - i] == *r);
        ++i;
    }

    BOOST_TEST(i == expected.size());
}


BOOST_AUTO_TEST_CASE(line_iter_forwards) {
    const char* const source = "A\n"
                               "b\n"
                               "c\n";
    utils::LineIter iter = [&]() {
        auto r = utils::LineIter::make(source, source + strlen(source) + 1);
        BOOST_TEST(r.has_value());
        return *r;
    }();

    {
        std::array<const char*, 4> expected { { "A\n", "b\n", "c\n", "" } };

        for (const char* e : expected) {
            const auto r = iter.forwards();
            BOOST_TEST(r.has_value());
            const auto [ptr, sz, line, end_in_new_line] = *r;
            for (size_t i { 0 }; i < sz; ++i) {
                BOOST_TEST(ptr[i] == e[i]);
            }
        }
        BOOST_TEST(!iter.forwards().has_value());
    }
    {
        std::array<const char*, 4> expected { { "", "c\n", "b\n", "A\n" } };

        for (const char* e : expected) {
            const auto r = iter.backwards();
            BOOST_TEST(r.has_value());
            const auto [ptr, sz, line, end_in_new_line] = *r;
            for (size_t i { 0 }; i < sz; ++i) {
                BOOST_TEST(ptr[i] == e[i]);
            }
        }
        BOOST_TEST(!iter.backwards().has_value());
    }
}

BOOST_AUTO_TEST_CASE(line_iter_backwards) {
    const char* const source = "A\n"
                               "b\n"
                               "c\n";
    utils::LineIter iter = [&]() {
        const auto len = strlen(source);
        auto r = utils::LineIter::make(source, source + len + 1, { len + 1, 2, 2 });
        BOOST_TEST(r.has_value());
        return *r;
    }();


    {
        std::array<const char*, 4> expected { { "", "c\n", "b\n", "A\n" } };

        for (const char* e : expected) {
            const auto r = iter.backwards();
            BOOST_TEST(r.has_value());
            const auto [ptr, sz, line, end_in_new_line] = *r;
            for (size_t i { 0 }; i < sz; ++i) {
                BOOST_TEST(ptr[i] == e[i]);
            }
        }
        BOOST_TEST(!iter.backwards().has_value());
    }

    {
        std::array<const char*, 4> expected { { "A\n", "b\n", "c\n", "" } };

        for (const char* e : expected) {
            const auto r = iter.forwards();
            BOOST_TEST(r.has_value());
            const auto [ptr, sz, line, end_in_new_line] = *r;
            for (size_t i { 0 }; i < sz; ++i) {
                BOOST_TEST(ptr[i] == e[i]);
            }
        }
        BOOST_TEST(!iter.forwards().has_value());
    }
}


static constexpr std::array default_gobble {
    TokenType::Space, TokenType::NewLine, TokenType::Tab, TokenType::Comment, TokenType::MultilineComment,
};

template <size_t N, size_t M>
void match(const char* text, const std::array<TokenType, N>& to_find, const std::array<TokenType, M>& to_gobble) {
    const auto text_len = strlen(text);

    CodePointStream stream { text, text_len, {} };

    sc::lex::actions::TypeAndLocationAction action {};

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
              TokenType::Space,
              TokenType::Name,
              TokenType::Space,
              TokenType::Float,
              TokenType::Space,
              TokenType::SymbolSlash,
              TokenType::Space,
              TokenType::NewLine,
              TokenType::Space,
              TokenType::SymbolQuote,
              TokenType::SemiColon,
              TokenType::Space,
              TokenType::NewLine,
              TokenType::Minus,
              TokenType::Float,
              TokenType::Space,
              TokenType::Tab,
              TokenType::Space,
              TokenType::PrimitiveName,
              TokenType::Space,
              TokenType::ClassName,
              TokenType::Space,
              TokenType::KeywordBinaryOperator,
              TokenType::SymbolSlash,
              TokenType::OpenParen,
              TokenType::Space,
              TokenType::EndOfFile,
          },
          std::array<TokenType, 0> {});

    match("    *new ", std::array { TokenType::Multiply, TokenType::Name }, default_gobble);

    match("    const nl = \"\\n\"; \n\t*new ",
          std::array {
              TokenType::Const,
              TokenType::Name,
              TokenType::EqualsSign,
              TokenType::StringLine,
              TokenType::SemiColon,
              TokenType::Multiply,
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
              TokenType::EqualsSign,
              TokenType::OpenCurly,
              TokenType::Pipe,
              TokenType::Name,
              TokenType::Comma,
              TokenType::Name,
              TokenType::Comma,
              TokenType::Name,
              TokenType::Pipe,
              TokenType::Name,
              TokenType::Add,
              TokenType::Name,
              TokenType::Add,
              TokenType::Name,
              TokenType::CloseCurly,
              TokenType::SemiColon,
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

BOOST_AUTO_TEST_CASE(symbol) {
    match("\\)", std::array { TokenType::SymbolSlash, TokenType::CloseParen }, default_gobble);
}

BOOST_AUTO_TEST_CASE(ascii) {
    match("$a", std::array { TokenType::Ascii }, default_gobble);
    match("$a)", std::array { TokenType::Ascii, TokenType::CloseParen }, default_gobble);
    match("$\\n", std::array { TokenType::Ascii }, default_gobble);
    match("$\\n)", std::array { TokenType::Ascii, TokenType::CloseParen }, default_gobble);
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
              T::ClassName,  TokenType::OpenCurly, 
              T::ClassVar,
              TokenType::LessThan, T::Name, TokenType::Comma,
              T::Name, TokenType::Comma,
              T::Name,TokenType::Comma, 
              TokenType::LessThan, T::Name,TokenType::SemiColon, 
              T::Const, T::Name, TokenType::EqualsSign, T::StringLine, TokenType::SemiColon,
              TokenType::Multiply, T::Name, TokenType::OpenCurly,
              T::Arg, T::Name, TokenType::EqualsSign, T::Integer, TokenType::SemiColon,
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
              T::ClassName,  TokenType::OpenCurly, 
              T::Var,
              TokenType::LessThan, T::Name, TokenType::Comma,
              T::ReadWriteVar, T::Name, TokenType::SemiColon,

              T::Var,
              TokenType::GreaterThan, T::Name, TokenType::Comma,
              TokenType::GreaterThan, T::Name, TokenType::Comma,
              TokenType::GreaterThan, T::Name, TokenType::SemiColon,

              T::Var,
              T::Name, TokenType::Comma,
              T::Name, TokenType::Comma,
              T::Name, TokenType::SemiColon,
          },
          default_gobble);
    // clang-format on
}
