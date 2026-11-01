#include "sc_query/hooks.hpp"

#ifdef SC_LSP_CONNECTED
#    include <boost/asio/io_context.hpp>
#    include <boost/asio/ip/udp.hpp>
#endif

#include <algorithm>
#include <array>
#include <cstring>
#include <map>

namespace sc::query {

namespace {

////////////////////////////////////////////////////////////////////////////////
// Disconnected
////////////////////////////////////////////////////////////////////////////////

class NullHookHost final : public HookHost {
public:
    explicit NullHookHost(std::string why): m_why(std::move(why)) {}
    bool connected() override { return false; }
    std::string status() override { return m_why; }
    std::vector<HookCompletion> complete_symbol(const Library&, const SymbolContext&) override { return {}; }
    std::optional<Json> call(const Library&, std::string_view, std::string_view, const Json&) override {
        return std::nullopt;
    }
    EvalResult evaluate(const std::string&) override { return { false, m_why }; }
    std::string stale_reason(const Library&, HookId) override { return m_why; }

private:
    std::string m_why;
};

#ifdef SC_LSP_CONNECTED

////////////////////////////////////////////////////////////////////////////////
// OSC, only what the messages here need: int32, float32 and string arguments
////////////////////////////////////////////////////////////////////////////////

struct OscArg {
    char type; // 'i', 'f' or 's'
    std::int32_t i { 0 };
    float f { 0 };
    std::string s {};
};

void pad(std::string& out) {
    do
        out += '\0';
    while (out.size() % 4 != 0);
}

void put_int(std::string& out, std::uint32_t v) {
    for (int shift = 24; shift >= 0; shift -= 8)
        out += static_cast<char>((v >> shift) & 0xFF);
}

std::string encode(const std::string& address, const std::vector<OscArg>& args) {
    std::string out = address;
    pad(out);
    std::string tags = ",";
    for (const auto& a : args)
        tags += a.type;
    out += tags;
    pad(out);
    for (const auto& a : args) {
        if (a.type == 'i') {
            put_int(out, static_cast<std::uint32_t>(a.i));
        } else if (a.type == 'f') {
            std::uint32_t bits;
            std::memcpy(&bits, &a.f, sizeof bits);
            put_int(out, bits);
        } else {
            out += a.s;
            pad(out);
        }
    }
    return out;
}

std::optional<std::string> read_string(std::string_view data, std::size_t& at) {
    const auto end = data.find('\0', at);
    if (end == std::string_view::npos)
        return std::nullopt;
    std::string s { data.substr(at, end - at) };
    at = (end + 4) & ~std::size_t { 3 };
    return s;
}

std::uint32_t get_int(std::string_view data, std::size_t at) {
    std::uint32_t v = 0;
    for (std::size_t k = 0; k < 4; ++k)
        v = (v << 8) | static_cast<unsigned char>(data[at + k]);
    return v;
}

std::optional<std::pair<std::string, std::vector<OscArg>>> decode(std::string_view data) {
    std::size_t at = 0;
    auto address = read_string(data, at);
    if (!address || at > data.size())
        return std::nullopt;
    auto tags = read_string(data, at);
    if (!tags || tags->empty() || (*tags)[0] != ',')
        return std::nullopt;
    std::vector<OscArg> args;
    for (const char t : tags->substr(1)) {
        OscArg a { t };
        if (t == 'i' || t == 'f') {
            if (at + 4 > data.size())
                return std::nullopt;
            const auto v = get_int(data, at);
            at += 4;
            if (t == 'i')
                a.i = static_cast<std::int32_t>(v);
            else
                std::memcpy(&a.f, &v, sizeof v);
        } else if (t == 's' || t == 'S') {
            auto s = read_string(data, at);
            if (!s)
                return std::nullopt;
            a.type = 's';
            a.s = std::move(*s);
        } else if (t == 'b') {
            if (at + 4 > data.size())
                return std::nullopt;
            const auto n = get_int(data, at);
            at += 4;
            if (at + n > data.size())
                return std::nullopt;
            a.type = 's';
            a.s = std::string { data.substr(at, n) };
            at = (at + n + 3) & ~std::size_t { 3 };
        } else {
            // Types that carry no data, e.g., T, F, N.
            a.type = t;
        }
        args.push_back(std::move(a));
    }
    return std::make_pair(std::move(*address), std::move(args));
}

OscArg osc_int(std::int32_t i) { return { 'i', i }; }
OscArg osc_string(std::string s) { return { 's', 0, 0, std::move(s) }; }

////////////////////////////////////////////////////////////////////////////////
// Connected to sclang
////////////////////////////////////////////////////////////////////////////////

class OscHookHost final : public HookHost {
public:
    explicit OscHookHost(const HookHostConfig& config):
        m_config(config),
        m_socket(m_io, boost::asio::ip::udp::endpoint(boost::asio::ip::udp::v4(), 0)),
        m_target(boost::asio::ip::make_address(config.host), config.port) {}

    bool connected() override {
        const auto now = std::chrono::steady_clock::now();
        const auto retry = m_connected ? std::chrono::seconds { 5 } : std::chrono::seconds { 3 };
        if (m_checked && now - *m_checked < retry)
            return m_connected;
        m_checked = now;
        const auto reply = exchange("/lsp/hello", {});
        m_connected = reply && reply->first == "ok";
        m_sclang_hooks.clear();
        m_sclang_allows_eval = false;
        if (m_connected) {
            if (const auto info = Json::parse(reply->second)) {
                m_sclang_allows_eval = (*info)["allowEval"].boolean();
                for (const auto& [name, hook] : (*info)["hooks"].members())
                    m_sclang_hooks[name] = static_cast<std::uintmax_t>(hook["size"].integer());
            }
        }
        return m_connected;
    }

    std::string status() override {
        const auto where = m_config.host + ":" + std::to_string(m_config.port);
        if (!connected())
            return "sclang isn't answering on " + where + " (is LSPHook in its class library?)";
        return "connected to sclang on " + where + ", " + std::to_string(m_sclang_hooks.size()) + " hooks"
            + (m_sclang_allows_eval ? ", eval allowed by sclang" : "");
    }

    std::string stale_reason(const Library& library, HookId id) override {
        if (!connected())
            return "sclang isn't running";
        const auto& hook = library.hooks()[id];
        const auto& name = library.get(hook.hook_class).name;
        const auto fnd = m_sclang_hooks.find(name);
        if (fnd == m_sclang_hooks.end())
            return "sclang hasn't compiled " + name + ", recompile its class library";
        if (fnd->second != hook.file_size)
            return "sclang compiled a different " + hook.file.filename().u8string() + ", recompile its class library";
        return {};
    }

    std::optional<Json> call(const Library& library, std::string_view hook_class, std::string_view selector,
                             const Json& argument) override {
        const auto c = library.find_class(hook_class);
        if (!c || !library.get(*c).hook || !stale_reason(library, *library.get(*c).hook).empty())
            return std::nullopt;
        std::vector<OscArg> args { osc_string(std::string { hook_class }), osc_string(std::string { selector }) };
        // The argument is an Event, as key value pairs.
        for (const auto& [key, value] : argument.members()) {
            args.push_back(osc_string(key));
            if (value.type() == Json::Type::Int)
                args.push_back(osc_int(static_cast<std::int32_t>(value.integer())));
            else if (value.type() == Json::Type::Double)
                args.push_back({ 'f', 0, static_cast<float>(value.number()) });
            else
                args.push_back(osc_string(value.is_string() ? value.str() : value.dump()));
        }
        const auto reply = exchange("/lsp/call", std::move(args));
        if (!reply || reply->first != "ok")
            return std::nullopt;
        return Json::parse(reply->second);
    }

    std::vector<HookCompletion> complete_symbol(const Library& library, const SymbolContext& context) override {
        std::vector<HookCompletion> out;
        if (!connected())
            return out;
        Json event = Json::object();
        if (!context.receiver.empty())
            event["receiver"] = context.receiver;
        if (!context.selector.empty())
            event["selector"] = context.selector;
        if (context.argument >= 0)
            event["argument"] = context.argument;
        if (!context.previous.empty())
            event["previous"] = context.previous;
        if (!context.keyword.empty())
            event["keyword"] = context.keyword;
        event["prefix"] = context.prefix;
        for (const auto& hook : library.hooks().raw()) {
            if (std::find(hook.selectors.begin(), hook.selectors.end(), "completeSymbol") == hook.selectors.end())
                continue;
            const auto& name = library.get(hook.hook_class).name;
            const auto result = call(library, name, "completeSymbol", event);
            if (!result)
                continue;
            for (const auto& item : result->items()) {
                if (item.is_string())
                    out.push_back({ item.str(), {}, name });
                else if (item.is_object() && item["label"].is_string())
                    out.push_back({ item["label"].str(), item["detail"].str(), name });
            }
        }
        return out;
    }

    EvalResult evaluate(const std::string& code) override {
        if (!m_config.allow_eval)
            return { false, "evaluating code isn't enabled" };
        if (!connected())
            return { false, "sclang isn't running, or LSPHook isn't in its class library" };
        if (!m_sclang_allows_eval)
            return { false, "sclang refuses to evaluate code, set LSPHook.allowEval = true in sclang to allow it" };
        // Evaluating may take a while, e.g., booting the server.
        const auto reply = exchange("/lsp/eval", { osc_string(code) }, std::chrono::seconds { 30 });
        if (!reply)
            return { false, "sclang didn't answer in time" };
        return { reply->first == "ok", reply->second };
    }

private:
    // Sends a request with a fresh id and waits for `/lsp/reply id status text`.
    std::optional<std::pair<std::string, std::string>>
    exchange(const std::string& address, std::vector<OscArg> args,
             std::optional<std::chrono::milliseconds> timeout = std::nullopt) {
        const auto id = ++m_next_id;
        args.insert(args.begin(), osc_int(id));
        const auto packet = encode(address, args);
        boost::system::error_code ec;
        m_socket.send_to(boost::asio::buffer(packet), m_target, 0, ec);
        if (ec)
            return std::nullopt;
        const auto deadline = std::chrono::steady_clock::now() + timeout.value_or(m_config.timeout);
        while (std::chrono::steady_clock::now() < deadline) {
            std::optional<std::size_t> received;
            boost::asio::ip::udp::endpoint sender;
            m_socket.async_receive_from(boost::asio::buffer(m_buffer), sender,
                                        [&](const boost::system::error_code& e, std::size_t n) {
                                            if (!e)
                                                received = n;
                                        });
            m_io.restart();
            m_io.run_until(deadline);
            if (!received) {
                m_socket.cancel(ec);
                m_io.restart();
                m_io.run();
                return std::nullopt;
            }
            const auto message = decode(std::string_view { m_buffer.data(), *received });
            if (!message || message->first != "/lsp/reply" || message->second.size() < 2)
                continue;
            const auto& reply = message->second;
            if (reply[0].type != 'i' || reply[0].i != id)
                continue; // a late reply to an earlier request
            return std::make_pair(reply[1].s, reply.size() > 2 ? reply[2].s : std::string {});
        }
        return std::nullopt;
    }

    HookHostConfig m_config;
    boost::asio::io_context m_io {};
    boost::asio::ip::udp::socket m_socket;
    boost::asio::ip::udp::endpoint m_target;
    std::array<char, 65536> m_buffer {};
    std::int32_t m_next_id { 0 };
    std::optional<std::chrono::steady_clock::time_point> m_checked {};
    bool m_connected { false };
    bool m_sclang_allows_eval { false };
    std::map<std::string, std::uintmax_t> m_sclang_hooks {};
};
#endif

}

bool hooks_supported() {
#ifdef SC_LSP_CONNECTED
    return true;
#else
    return false;
#endif
}

std::unique_ptr<HookHost> make_hook_host(const HookHostConfig& config) {
#ifdef SC_LSP_CONNECTED
    if (config.host != "127.0.0.1" && config.host != "localhost")
        return std::make_unique<NullHookHost>("hooks only connect to sclang on this machine (127.0.0.1)");
    auto local = config;
    local.host = "127.0.0.1";
    try {
        return std::make_unique<OscHookHost>(local);
    } catch (const std::exception& e) {
        return std::make_unique<NullHookHost>(std::string { "could not open a socket for sclang: " } + e.what());
    }
#else
    (void)config;
    return std::make_unique<NullHookHost>("this build runs no hooks (built without SC_LSP_CONNECTED)");
#endif
}

}
