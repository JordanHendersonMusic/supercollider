#pragma once

#include "details/codepoint_stream.hpp"
#include "details/tokens.hpp"

namespace sc::lex {

/// @brief Reads the next token from the stream.
///
/// Never fails: invalid input becomes a token with an error type, see is_error().
/// For each token found, calls `action.process<TokenType::X>(range)`. If that returns a value, it is returned. If it
/// returns `std::nullopt`, the token is dropped and the next one is read. The action must return a value for
/// `TokenType::EndOfFile`.
///
/// @tparam Action A type with an `Output` type and a templated `process` method, see sc::lex::actions.
/// @param stream The code points to read, advanced past the token.
/// @param action Builds the output from a token's type and range.
/// @return The first token the action did not drop.

template <typename Action> auto lexer(CodePointStream& stream, Action& action) noexcept -> typename Action::Output;
}

// Includes implementation after forward declare.
// Bit unusual to have a header at the bottom, but you don't need to know the details of how it works to use it.
#include "details/lexer_impl.hpp"
