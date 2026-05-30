#include "text_location.hpp"
#define BOOST_TEST_MODULE sc_lexer_tests
#include "tokens.hpp"
#include <boost/test/included/unit_test.hpp>
#include <cstddef>
#include <source_utils.hpp>
#include <lexer.hpp>

using namespace sc::lex;
// MSVC needs this.
using T = sc::lex::TokenType;

BOOST_TEST_DONT_PRINT_LOG_VALUE(sc::lex::TokenType);
BOOST_TEST_DONT_PRINT_LOG_VALUE(sc::lex::SourceCodeLocation);
BOOST_TEST_DONT_PRINT_LOG_VALUE(sc::lex::SourceCodeRange);

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
    T::Space, T::NewLine, T::Tab, T::Comment, T::MultilineComment,
};

template <size_t N, size_t M>
void match(const char* text, const std::array<T, N>& to_find, const std::array<T, M>& to_gobble, bool print = false) {
    if (print)
        std::cout << "MATCHING: " << text << std::endl;
    const auto text_len = strlen(text);

    CodePointStream stream { text, text_len, {} };

    sc::lex::actions::TypeAndLocationAction action {};

    for (const T t : to_find) {
        auto o = lexer(stream, action);

        while (std::find(to_gobble.begin(), to_gobble.end(), o.type) != to_gobble.end()) {
            o = lexer(stream, action);
        }

        if (print)
            std::cout << to_string(o.type) << " ";

        BOOST_TEST(o.type == t);
    }

    auto o = lexer(stream, action);
    while (std::find(to_gobble.begin(), to_gobble.end(), o.type) != to_gobble.end() && o.type != T::EndOfFile) {
        o = lexer(stream, action);
    }
    if (print)
        std::cout << to_string(o.type) << " ";

    BOOST_TEST(o.type == T::EndOfFile);

    if (print) {
        std::cout << std::endl;
        std::cout << std::endl;
    }
}

BOOST_AUTO_TEST_CASE(basic) {
    const char* text = "   some 0.312 \\hello \n 'text';  \n"
                       "\n"
                       "\n"
                       "-0.2 \t  _Pri Foo _T:\\6( ";

    // NO gobble
    match(text,
          std::array {
              T::Space,
              T::Name,
              T::Space,
              T::Float,
              T::Space,
              T::SymbolSlash,
              T::Space,
              T::NewLine,
              T::Space,
              T::SymbolQuote,
              T::SemiColon,
              T::Space,
              T::NewLine,
              T::Minus,
              T::Float,
              T::Space,
              T::Tab,
              T::Space,
              T::PrimitiveName,
              T::Space,
              T::ClassName,
              T::Space,
              T::KeywordBinaryOperator,
              T::SymbolSlash,
              T::OpenParen,
              T::Space,
              T::EndOfFile,
          },
          std::array<T, 0> {});

    match("    *new ", std::array { T::Multiply, T::Name }, default_gobble);

    match("    const nl = \"\\n\"; \n\t*new ",
          std::array {
              T::Const,
              T::Name,
              T::EqualsSign,
              T::StringLine,
              T::SemiColon,
              T::Multiply,
              T::Name,
          },
          default_gobble);
}

BOOST_AUTO_TEST_CASE(fn) {
    const char* text = "       \t\t\t\t\t\t        var f = {|a,b,c|\na+b + c}\n\n\t;   ";

    match(text,
          std::array {
              T::Var,
              T::Name,
              T::EqualsSign,
              T::OpenCurly,
              T::Pipe,
              T::Name,
              T::Comma,
              T::Name,
              T::Comma,
              T::Name,
              T::Pipe,
              T::Name,
              T::Add,
              T::Name,
              T::Add,
              T::Name,
              T::CloseCurly,
              T::SemiColon,
          },
          default_gobble);
}

BOOST_AUTO_TEST_CASE(strings) {
    match(R"%(   "(\""   )%", std::array { T::StringLine }, default_gobble);
    match(R"%( "(\"" )%", std::array { T::StringLine }, default_gobble);
    match(R"%( "\")" abs )%", std::array { T::StringLine, T::Name }, default_gobble);
    match(R"%( "◎" bang )%", std::array { T::StringLine, T::Name }, default_gobble);
    match(R"%( 
			"The function % should behave the same for a PatternProxy and its source:\n%\n"
    )%",
          std::array { T::StringLine }, default_gobble);
}

BOOST_AUTO_TEST_CASE(symbol) { match("\\)", std::array { T::SymbolSlash, T::CloseParen }, default_gobble); }

BOOST_AUTO_TEST_CASE(ascii) {
    match("$a", std::array { T::Ascii }, default_gobble);
    match("$a)", std::array { T::Ascii, T::CloseParen }, default_gobble);
    match("$n", std::array { T::Ascii }, default_gobble);
    match("$\n", std::array { T::ErFoundInvalidNewlineCharaceter }, default_gobble);
    match("$\\n)", std::array { T::Ascii, T::CloseParen }, default_gobble);
    match("$\\\n)", std::array { T::ErFoundInvalidNewlineCharaceter, T::CloseParen }, default_gobble);

    match("$\r\n)", std::array { T::ErFoundInvalidNewlineCharaceter, T::CloseParen }, default_gobble);
    match("$\\\r\n)", std::array { T::ErFoundInvalidNewlineCharaceter, T::CloseParen }, default_gobble);

    match("$\v)", std::array { T::ErFoundInvalidNewlineCharaceter, T::CloseParen }, default_gobble);
    match("$\\\v)", std::array { T::ErFoundInvalidNewlineCharaceter, T::CloseParen }, default_gobble);

    match("$ ", std::array { T::Ascii }, default_gobble);
    match("$\\ ", std::array { T::Ascii }, default_gobble);
    match("$    bang  ", std::array { T::Ascii, T::Name }, default_gobble);
}

BOOST_AUTO_TEST_CASE(larger_obj) {
    const auto txt = R"%%(
Object {
	classvar <dependantsDictionary, currentEnvironment, topEnvironment, <uniqueMethods;

	const nl = "\n";

	*new { arg maxSize = 0; _BasicNew
    )%%";
    // clang-format off
    match(txt,
          std::array {
              T::ClassName,  T::OpenCurly, 
              T::ClassVar,
              T::LessThan, T::Name, T::Comma,
              T::Name, T::Comma,
              T::Name,T::Comma, 
              T::LessThan, T::Name,T::SemiColon, 
              T::Const, T::Name, T::EqualsSign, T::StringLine, T::SemiColon,
              T::Multiply, T::Name, T::OpenCurly,
              T::Arg, T::Name, T::EqualsSign, T::Integer, T::SemiColon,
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
    // clang-format off
    match(txt,
          std::array {
              T::ClassName,  T::OpenCurly, 
              T::Var,
              T::LessThan, T::Name, T::Comma,
              T::ReadWriteVar, T::Name, T::SemiColon,

              T::Var,
              T::GreaterThan, T::Name, T::Comma,
              T::GreaterThan, T::Name, T::Comma,
              T::GreaterThan, T::Name, T::SemiColon,

              T::Var,
              T::Name, T::Comma,
              T::Name, T::Comma,
              T::Name, T::SemiColon,
          },
          default_gobble);
    // clang-format on
}

BOOST_AUTO_TEST_CASE(unicode_comments_1) {
    const auto txt = R"%%(// delta is only used to compute § )%%";
    match(txt, std::array { T::Comment }, std::array<T, 0> {});
}

BOOST_AUTO_TEST_CASE(unicode_comments_2) {
    const auto txt = R"%%(// © 2003 Lance Putnam
1)%%";
    match(txt, std::array { T::Comment, T::NewLine, T::Integer }, std::array<T, 0> {});
}


BOOST_AUTO_TEST_CASE(bare_carriage) {
    const auto bare_carriage = "foo bar \r 1";
    match(bare_carriage,
          std::array { T::Name, T::Space, T::Name, T::Space, T::ErFoundInvalidNewlineCharaceter, T::Space, T::Integer },
          std::array<T, 0> {});
}

BOOST_AUTO_TEST_CASE(windows_new_line) {
    const auto windows_new_line = "foo bar \r\n 1";
    match(windows_new_line, std::array { T::Name, T::Space, T::Name, T::Space, T::NewLine, T::Space, T::Integer },
          std::array<T, 0> {});
}

BOOST_AUTO_TEST_CASE(invalid_new_line) {
    const auto invalid_new_line = "\v\f";
    match(invalid_new_line, std::array { T::ErFoundInvalidNewlineCharaceter, T::ErFoundInvalidNewlineCharaceter },
          std::array<T, 0> {});
}

BOOST_AUTO_TEST_CASE(line_comment) {
    match("// I am a comment\nfoo", std::array { T::Comment, T::NewLine, T::Name }, std::array<T, 0> {});

    match("// I am a comment\r\nfoo", std::array { T::Comment, T::NewLine, T::Name }, std::array<T, 0> {});

    match("// I am a comment\rfoo", std::array { T::ErFoundInvalidNewlineCharaceter, T::Name }, std::array<T, 0> {});

    match("// I am a comment\vfoo", std::array { T::ErFoundInvalidNewlineCharaceter, T::Name }, std::array<T, 0> {});

    match("// I am a comment\ffoo", std::array { T::ErFoundInvalidNewlineCharaceter, T::Name }, std::array<T, 0> {});
}


BOOST_AUTO_TEST_CASE(quote_symbol) {
    // Yes you can escape the new line character.
    match("''", std::array { T::SymbolQuote }, std::array<T, 0> {});

    match("'symbol\\\nabc'", std::array { T::SymbolQuote }, std::array<T, 0> {});

    match("'symbol\\\r\nabc'", std::array { T::SymbolQuote }, std::array<T, 0> {});

    match("'symbol\\\rabc'", std::array { T::ErFoundInvalidNewlineCharaceter, T::Name, T::ErSymbolQuoteUnclosed },
          std::array<T, 0> {});

    match("'symbol\\\vabc'", std::array { T::ErFoundInvalidNewlineCharaceter, T::Name, T::ErSymbolQuoteUnclosed },
          std::array<T, 0> {});

    match("'symbol\\\fabc'", std::array { T::ErFoundInvalidNewlineCharaceter, T::Name, T::ErSymbolQuoteUnclosed },
          std::array<T, 0> {});

    match("'symbol\\fabc'", std::array { T::SymbolQuote }, std::array<T, 0> {});
}


BOOST_AUTO_TEST_CASE(new_lines) {
    match("foo\r\nbar", std::array { T::Name, T::NewLine, T::Name }, std::array<T, 0> {});
    match("foo\nbar", std::array { T::Name, T::NewLine, T::Name }, std::array<T, 0> {});
    //
    match("foo\vbar", std::array { T::Name, T::ErFoundInvalidNewlineCharaceter, T::Name }, std::array<T, 0> {});
    match("foo\fbar", std::array { T::Name, T::ErFoundInvalidNewlineCharaceter, T::Name }, std::array<T, 0> {});
    match("foo\rbar", std::array { T::Name, T::ErFoundInvalidNewlineCharaceter, T::Name }, std::array<T, 0> {});


    match("\"foo\r\nbar\"", std::array { T::StringLine }, std::array<T, 0> {});
    match("\"foo\nbar\"", std::array { T::StringLine }, std::array<T, 0> {});
    //
    match("\"foo\vbar\"", std::array { T::ErFoundInvalidNewlineCharaceter, T::Name, T::ErStringUnclosed },
          std::array<T, 0> {});
    match("\"foo\fbar\"", std::array { T::ErFoundInvalidNewlineCharaceter, T::Name, T::ErStringUnclosed },
          std::array<T, 0> {});
    match("\"foo\rbar\"", std::array { T::ErFoundInvalidNewlineCharaceter, T::Name, T::ErStringUnclosed },
          std::array<T, 0> {});
}


template <std::size_t N>
void check_newline(const char* txt, std::array<sc::lex::SourceCodeRange, N> locs, bool print = false) {
    const auto text_len = strlen(txt);

    if (print)
        std::cout << "check_newline: " << txt << "\n";

    CodePointStream stream { txt, text_len, {} };

    sc::lex::actions::TypeAndLocationAction action {};
    const auto print_scrange = [](const sc::lex::SourceCodeRange& r) {
        std::cout << "begin[ " << r.begin.absolute << ':' << r.begin.lineNumber << ':' << r.begin.column << "], end["
                  << r.end.absolute << ':' << r.end.lineNumber << ':' << r.end.column << "]";
    };

    for (auto l : locs) {
        const auto o = lexer(stream, action);

        if (print) {
            std::cout << to_string(o.type);
            std::cout << " \tgot: ";
            print_scrange(o.range);
            std::cout << " \texpected: ";
            print_scrange(l);
            std::cout << "\n";
        }
        BOOST_TEST(o.range == l);
    }
    const auto o = lexer(stream, action);
    BOOST_TEST(o.type == sc::lex::TokenType::EndOfFile);
}

BOOST_AUTO_TEST_CASE(new_checks) {
    using L = sc::lex::SourceCodeRange;

    check_newline("\"meow\nwoof\";\n1+1;",
                  std::array {
                      // string
                      L { { 0, 0, 0 }, { 11, 1, 5 } },
                      //;
                      L { { 11, 1, 5 }, { 12, 1, 6 } },
                      //\n
                      L { { 12, 1, 6 }, { 13, 2, 0 } },
                      // 1
                      L { { 13, 2, 0 }, { 14, 2, 1 } },
                      // +
                      L { { 14, 2, 1 }, { 15, 2, 2 } },
                      // 1
                      L { { 15, 2, 2 }, { 16, 2, 3 } },
                      // ;
                      L { { 16, 2, 3 }, { 17, 2, 4 } },
                      // EOF
                      L { { 17, 2, 4 }, { 17, 2, 4 } },
                  });
}
