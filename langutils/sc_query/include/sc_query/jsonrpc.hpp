#pragma once

/// @file
/// @brief JSON-RPC 2.0 over a pair of streams, shared by sc_lsp and sc_mcp.

#include "sc_query/json.hpp"

#include <iosfwd>
#include <mutex>
#include <optional>
#include <string>

namespace sc::query::rpc {

////////////////////////////////////////////////////////////////////////////////
// API
////////////////////////////////////////////////////////////////////////////////

/// @brief How messages are separated.
enum struct Framing : std::uint8_t {
    ContentLength, ///< LSP: a `Content-Length: n` header, a blank line, then n bytes.
    NewlineDelimited, ///< MCP over stdio: one message per line.
};

/// @brief The error codes JSON-RPC and the LSP define.
namespace error_code {
inline constexpr int parse_error = -32700;
inline constexpr int invalid_request = -32600;
inline constexpr int method_not_found = -32601;
inline constexpr int invalid_params = -32602;
inline constexpr int internal_error = -32603;
inline constexpr int server_not_initialized = -32002;
inline constexpr int request_cancelled = -32800;
}

/// @brief Reads and writes messages. Writing is safe from several threads, reading is from one.
class Transport {
public:
    Transport(std::istream& in, std::ostream& out, Framing framing): m_in(in), m_out(out), m_framing(framing) {}

    /// @brief Reads the next message.
    /// @return The message, or nothing at the end of the input. A message that isn't JSON is answered with a parse
    /// error and skipped.
    [[nodiscard]] std::optional<Json> read();
    /// @brief Writes a message.
    void write(const Json& message);

    /// @brief Answers a request.
    void respond(const Json& id, Json result);
    /// @brief Answers a request with an error.
    void respond_error(const Json& id, int code, std::string message);
    /// @brief Sends a notification.
    void notify(std::string method, Json params);
    /// @brief Sends a request, which is answered by a response the reader sees.
    void request(const Json& id, std::string method, Json params);

private:
    [[nodiscard]] std::optional<std::string> read_raw();

    std::istream& m_in;
    std::ostream& m_out;
    Framing m_framing;
    std::mutex m_write_lock {};
};

/// @brief Puts stdin and stdout in binary mode where that matters (Windows), so line ends aren't changed.
void use_binary_stdio();

}
