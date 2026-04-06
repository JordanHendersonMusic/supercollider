#include <sstream>
#include <fstream>
#include <iostream>
#include "../compare.hpp"
#include "lexer.hpp"

std::uint32_t compare_tokens(const char* text, bool print) {
    std::uint32_t num_errors { 0 };
    State old_state { text, strlen(text), 0, 0, 0, State::Nil {}, {}, 0 };

    CodePointStream stream { true, text, strlen(text), {} };
    TokenOnlyAction action {};

    OldInfo old_i { 0, 0, 0 };
    NewInfo new_i { TokenType::EndOfFile, 0, 0 };
    while (true) {
        if (num_errors > 0)
            return num_errors;
        if (old_i.end < new_i.end) {
            const auto old_t = old_lexer(old_state, false);
            old_i = { old_t, old_state.token_start, old_state.token_end };
            num_errors++;
            if (old_i.end < new_i.end) {
                old_i.printOn(std::cout, text);
                std::cout << std::endl;
                new_i.printOn(std::cout, text);
                std::cout << '\n' << std::endl;
                continue;
            }
        } else if (new_i.end < old_i.end) {
            const auto new_t = lexer(stream, action);
            new_i = { new_t.first, new_t.second.begin.absolute, new_t.second.end.absolute };
            num_errors++;
            if (new_i.end < old_i.end) {
                old_i.printOn(std::cout, text);
                std::cout << std::endl;
                new_i.printOn(std::cout, text);
                std::cout << '\n' << std::endl;
                continue;
            }
        } else {
            const auto old_t = old_lexer(old_state, false);
            old_i = { old_t, old_state.token_start, old_state.token_end };
            const auto new_t = lexer(stream, action);
            new_i = { new_t.first, new_t.second.begin.absolute, new_t.second.end.absolute };
        }


        if (print) {
            old_i.printOn(std::cout, text);
            std::cout << std::endl;
            new_i.printOn(std::cout, text);
            std::cout << '\n' << std::endl;
        }

        if (old_i.type == YYEOF && new_i.type == TokenType::EndOfFile)
            break;
        if (old_i.type == YYEOF || new_i.type == TokenType::EndOfFile) {
            num_errors++;
            break;
        }

        if (old_i.type == STRING && old_i.end != new_i.end) {
            // new does string lines, which may be separated by white space.
            if (new_i.type != TokenType::StringLine) {
                num_errors++;
                std::cout << "String line failure" << std::endl;
            }

            while (true) {
                const auto n = lexer(stream, action);
                const NewInfo ni = { n.first, n.second.begin.absolute, n.second.end.absolute };

                // Exactly one multiline string, a normal string.
                if (ni.end == old_i.end)
                    break;

                // We've got many string lines
                if (ni.end < old_i.end) {
                    if (n.first != TokenType::StringLine) {
                        std::cout << "String line failure" << std::endl;
                        num_errors++;
                    }
                    continue;
                }


                if (ni.end > old_i.end) {
                    std::cout << "String too big" << std::endl;
                    num_errors++;
                    break;
                }
            };
            continue;
        } else {
            if (!tokens_equal(old_i.type, new_i.type)) {
                std::cout << "Token mismatch" << std::endl;
                old_i.printOn(std::cout, text);
                std::cout << std::endl;
                new_i.printOn(std::cout, text);
                std::cout << '\n' << std::endl;
                num_errors++;
            }
            continue;
        }
    }
    return num_errors;
}


int main(int argc, char** argv) {
    if (argc != 2) {
        std::cout << "first arg should be the file path" << std::endl;
        return 1;
    }
    std::ifstream t(argv[1]);
    std::stringstream buffer;
    buffer << t.rdbuf();
    const auto str = buffer.str();

    const auto r = compare_tokens(str.c_str(), false);

    if (r != 0) {
        std::cout << "Failed " << argv[1] << std::endl;
    }
    return r;
}
