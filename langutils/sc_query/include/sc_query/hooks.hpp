#pragma once

/// @file
/// @brief Running class library hooks, subclasses of LSPHook, in the user's running sclang.
///
/// Hooks are found statically (Library::hooks()), but only ever run by asking a running sclang over OSC. A build
/// without SC_LSP_CONNECTED runs no hooks at all. See SCClassLibrary/Common/Tooling/LSPHook.sc for the other side.

#include "sc_query/json.hpp"
#include "sc_query/library.hpp"

#include <chrono>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace sc::query {

////////////////////////////////////////////////////////////////////////////////
// API
////////////////////////////////////////////////////////////////////////////////

/// @brief Where a symbol is being typed, given to `completeSymbol` hooks as an Event.
struct SymbolContext {
    std::string receiver; ///< The class the enclosing call is sent to, e.g., `Pbind`, empty if not known.
    std::string selector; ///< The enclosing call's selector, e.g., `new` for `Pbind(`, empty if none.
    std::int32_t argument { -1 }; ///< The positional argument's index in the call, -1 if not in a call.
    std::string previous; ///< The text of the argument before this one, e.g., `\instrument`.
    std::string keyword; ///< The keyword when written `key: \`, empty if not.
    std::string prefix; ///< What has been typed of the symbol, without the backslash.
};

/// @brief A completion a hook gave.
struct HookCompletion {
    std::string label;
    std::string detail; ///< Shown beside it, may be empty.
    std::string hook; ///< The hook class that gave it.
};

/// @brief What evaluating code in sclang gave.
struct EvalResult {
    bool ok { false };
    std::string text; ///< The result's compile string, or the error.
};

/// @brief How to reach sclang.
struct HookHostConfig {
    std::string host { "127.0.0.1" }; ///< Only localhost is allowed, see make_hook_host().
    std::uint16_t port { 57120 }; ///< sclang's language port.
    std::chrono::milliseconds timeout { 400 }; ///< How long to wait for a reply.
    bool allow_eval { false }; ///< Whether evaluate() may be used, e.g., sc_mcp's `--allow-eval`.
};

/// @brief Runs hooks in a running sclang.
class HookHost {
public:
    virtual ~HookHost() = default;
    /// @brief Whether sclang answers. Asks again at most every few seconds while it doesn't.
    [[nodiscard]] virtual bool connected() = 0;
    /// @brief One line on the connection, for logs and tools.
    [[nodiscard]] virtual std::string status() = 0;
    /// @brief Asks every `completeSymbol` hook the library and sclang agree on. Empty when not connected.
    [[nodiscard]] virtual std::vector<HookCompletion> complete_symbol(const Library& library,
                                                                      const SymbolContext& context) = 0;
    /// @brief Calls one hook's class method with a plain data argument.
    [[nodiscard]] virtual std::optional<Json> call(const Library& library, std::string_view hook_class,
                                                   std::string_view selector, const Json& argument) = 0;
    /// @brief Evaluates code in sclang, which posts it first. Refused unless allowed here and by sclang
    /// (`LSPHook.allowEval = true`).
    [[nodiscard]] virtual EvalResult evaluate(const std::string& code) = 0;
    /// @brief Why a hook can't run, e.g., sclang compiled an older file, empty when it can.
    [[nodiscard]] virtual std::string stale_reason(const Library& library, HookId hook) = 0;
};

/// @brief Whether this build can run hooks, SC_LSP_CONNECTED.
[[nodiscard]] bool hooks_supported();

/// @brief A host for the configuration. Without SC_LSP_CONNECTED, or for a host other than localhost, one that is
/// never connected.
[[nodiscard]] std::unique_ptr<HookHost> make_hook_host(const HookHostConfig& config);

}
