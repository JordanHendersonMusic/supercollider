#include "lexer.hpp"
#include "../compare.hpp"

void print(OldInfo o, NewInfo n, const char* text) {
    o.printOn(std::cout, text);
    std::cout << std::endl;
    n.printOn(std::cout, text);
    std::cout << '\n' << std::endl;
}

void compare_tokens(const char* text, bool should_print = false) {
    if (should_print) {
        std::cout << text << "\n";
    }
    State old_state { text, strlen(text), 0, 0, 0, State::Nil {}, {}, 0 };

    CodePointStream stream { true, text, strlen(text), {} };
    TokenOnlyAction action {};

    OldInfo old_i { 0, 0, 0 };
    NewInfo new_i { TokenType::EndOfFile, 0, 0 };
    while (true) {
        if (old_i.end < new_i.end) {
            const auto old_t = old_lexer(old_state, false);
            old_i = { old_t, old_state.token_start, old_state.token_end };
            if (!should_print) {
                return compare_tokens(text, true);
            } else {
                print(old_i, new_i, text);
                __builtin_trap();
            }
            if (old_i.end < new_i.end)
                continue;
        } else if (new_i.end < old_i.end) {
            const auto new_t = lexer(stream, action);
            new_i = { new_t.first, new_t.second.begin.absolute, new_t.second.end.absolute };
            if (!should_print) {
                return compare_tokens(text, true);
            } else {
                print(old_i, new_i, text);
                __builtin_trap();
            }
            if (new_i.end < old_i.end)
                continue;
        } else {
            const auto old_t = old_lexer(old_state, false);
            old_i = { old_t, old_state.token_start, old_state.token_end };
            const auto new_t = lexer(stream, action);
            new_i = { new_t.first, new_t.second.begin.absolute, new_t.second.end.absolute };
        }


        if (should_print) {}

        if (old_i.type == YYEOF && new_i.type == TokenType::EndOfFile)
            break;
        if (old_i.type == YYEOF || new_i.type == TokenType::EndOfFile) {
            if (!should_print) {
                return compare_tokens(text, true);
            } else {
                print(old_i, new_i, text);
                __builtin_trap();
            }
            break;
        }

        if (old_i.type == STRING && old_i.end != new_i.end) {
            // new does string lines, which may be separated by white space.
            if (new_i.type != TokenType::StringLine) {
                if (!should_print) {
                    return compare_tokens(text, true);
                } else {
                    print(old_i, new_i, text);
                    __builtin_trap();
                }
            }
            while (true) {
                const auto n = lexer(stream, action);
                const NewInfo ni = { n.first, n.second.begin.absolute, n.second.end.absolute };

                if (ni.type != TokenType::StringLine) {
                    if (!should_print) {
                        return compare_tokens(text, true);
                    } else {
                        print(old_i, new_i, text);
                        __builtin_trap();
                    }
                }

                // Reached end.
                if (ni.end == old_i.end)
                    break;

                // We've got many string lines
                if (ni.end < old_i.end) {
                    if (n.first != TokenType::StringLine) {
                        if (!should_print) {
                            return compare_tokens(text, true);
                        } else {
                            print(old_i, new_i, text);
                            __builtin_trap();
                        }
                    }
                    continue;
                }


                if (ni.end > old_i.end)
                    __builtin_trap();
            };
            continue;
        } else {
            if (!tokens_equal(old_i.type, new_i.type)) {
                if (!should_print) {
                    return compare_tokens(text, true);
                } else {
                    print(old_i, new_i, text);
                    __builtin_trap();
                }
            }
            continue;
        }
    }
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size) {
    using namespace sc::lex;
    std::vector<char> bytes { data, data + size };
    bytes.push_back(' ');
    bytes.push_back(0);

    compare_tokens(bytes.data());

    return 0;
}
