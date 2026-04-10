#include <boost/test/unit_test.hpp>
#include <cstddef>
#include <random>

#include "../compare.hpp"

enum struct Print { No, Always, IfError };

void compare_tokens(const char* text, Print print) {
    State old_state { text, strlen(text), 0, 0, 0, State::Nil {}, {}, 0 };

    CodePointStream stream { true, text, strlen(text), {} };
    TokenOnlyAction action {};

    OldInfo old_i { 0, 0, 0 };
    NewInfo new_i { TokenType::EndOfFile, 0, 0 };

    const auto print_tokens = [&]() {
        old_i.printOn(std::cout, text);
        std::cout << std::endl;
        new_i.printOn(std::cout, text);
        std::cout << '\n' << std::endl;
    };
    while (true) {
        if (old_i.end < new_i.end) {
            const auto old_t = old_lexer(old_state, false);
            old_i = { old_t, old_state.token_start, old_state.token_end };
            BOOST_TEST(false);
            if (old_i.end < new_i.end)
                continue;
        } else if (new_i.end < old_i.end) {
            const auto new_t = lexer(stream, action);
            new_i = { new_t.first, new_t.second.begin.absolute, new_t.second.end.absolute };
            BOOST_TEST(false);
            if (new_i.end < old_i.end)
                continue;
        } else {
            const auto old_t = old_lexer(old_state, false);
            old_i = { old_t, old_state.token_start, old_state.token_end };
            const auto new_t = lexer(stream, action);
            new_i = { new_t.first, new_t.second.begin.absolute, new_t.second.end.absolute };
        }


        if (print == Print::Always)
            print_tokens();

        if (old_i.type == YYEOF && new_i.type == TokenType::EndOfFile)
            break;
        if (old_i.type == YYEOF || new_i.type == TokenType::EndOfFile) {
            BOOST_TEST(false);
            if (print != Print::No)
                print_tokens();

            break;
        }

        if (old_i.type == STRING && old_i.end != new_i.end) {
            // new does string lines, which may be separated by white space.
            BOOST_TEST(new_i.type == TokenType::StringLine);
            while (true) {
                const auto n = lexer(stream, action);
                const NewInfo ni = { n.first, n.second.begin.absolute, n.second.end.absolute };

                // Exactly one multiline string, a normal string.
                if (ni.end == old_i.end)
                    break;

                // We've got many string lines
                if (ni.end < old_i.end) {
                    BOOST_TEST(n.first == TokenType::StringLine);
                    continue;
                }


                BOOST_TEST_REQUIRE(ni.end <= old_i.end);
            };
            continue;
        } else if (old_i.type == BADTOKEN && new_i.type == TokenType::ErSymbolQuoteUnclosed) {
            // the new version doesn't consume any trailing newline characters, this produces better error messages.

            BOOST_TEST(old_i.start == new_i.start);
            if (old_i.end != new_i.end) {
                const auto c = text[old_i.end - 1];
                if (c == '\n' || c == '\r') {
                    new_i.end = old_i.end;
                } else {
                    // only reason they might differ is because the new version does not consume the new line character.
                    BOOST_TEST(false);
                    if (print != Print::No)
                        print_tokens();
                }
            }
        } else {
            BOOST_TEST(tokens_equal(old_i.type, new_i.type));
            if (print != Print::No && !tokens_equal(old_i.type, new_i.type))
                print_tokens();

            continue;
        }
    }
}

BOOST_AUTO_TEST_CASE(basic) {
    // Note the old lexer requires there be a space at the end of the file...
    compare_tokens("asdf 0.0 -0.2 pi const var arg 10pi <> | || + ++ <> < > 123ssss 324s43 ", Print::IfError);
}

void random_test_ascii(size_t seed, size_t sz) {
    std::mt19937 rng(seed);
    // This only tests valid ascii
    // Can't pass char or std::uint8_t (accept on gcc), each compiler provides different reasons and types.
    std::uniform_int_distribution<int> dist6(1, 127);


    std::string random {};
    random.reserve(sz);
    for (size_t i { 0 }; i < sz; ++i)
        random += static_cast<char>(dist6(rng));

    random += " ";

    compare_tokens(random.c_str(), Print::IfError);
}

BOOST_AUTO_TEST_CASE(random_test_all_ascii_few_big) {
    // If you find a seed that breaks, explicitly add it.
    for (size_t i { 0 }; i < 1'000; ++i)
        random_test_ascii(i, 100'000);
}


// Brute force, all 3 character combinations.
BOOST_AUTO_TEST_CASE(random_test_all_ascii_all_tiny) {
    char a[5];
    a[3] = ' ';
    a[4] = 0;
    for (std::uint32_t i { 1 }; i < 128; ++i) {
        a[0] = i;
        for (std::uint32_t j { 1 }; j < 128; ++j) {
            a[1] = j;
            for (std::uint32_t k { 1 }; k < 128; ++k) {
                a[2] = k;
                compare_tokens(a, Print::IfError);
            }
        }
    }
}
