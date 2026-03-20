
#include <boost/test/unit_test.hpp>
#include "lexer.hpp"

using namespace sc::lex;
using namespace sc::lex::literals;

CodePoint codepoint(const char* c) {
    const auto [cp, sz] = char_sequence_to_codepoint(c, 0, strlen(c));
    return cp;
}


BOOST_AUTO_TEST_CASE(unicode_tests) {
    BOOST_TEST(codepoint("a") == 'a');
    BOOST_TEST(codepoint("ab") == 'a');
    BOOST_TEST(codepoint("0") == '0');


    BOOST_TEST(codepoint("∀") == 0x2200);
    BOOST_TEST(codepoint("𝜠") == 0x1D720);
    BOOST_TEST(codepoint("Ͱ") == 0x370);
}
