#pragma once
#include <cstdint>

namespace sc::ast{
// Used as the semantic value when returning a token from the lexer in the bison parser.
struct LexerToken {};

// Defines <, >, and <> accessors on variables.
enum struct ReadWriteAccessor : std::uint8_t { Private, PublicRead, PublicWrite, PublicReadAndWrite };
}
