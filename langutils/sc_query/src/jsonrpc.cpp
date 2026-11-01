#include "sc_query/jsonrpc.hpp"

#include <cctype>
#include <cstdlib>
#include <istream>
#include <ostream>

#ifdef _WIN32
#    include <fcntl.h>
#    include <io.h>
#    include <stdio.h>
#endif

namespace sc::query::rpc {

std::optional<std::string> Transport::read_raw() {
    if (m_framing == Framing::NewlineDelimited) {
        std::string line;
        while (std::getline(m_in, line)) {
            if (!line.empty() && line.back() == '\r')
                line.pop_back();
            if (line.find_first_not_of(" \t") != std::string::npos)
                return line;
        }
        return std::nullopt;
    }
    std::size_t length = 0;
    bool has_length = false;
    std::string line;
    while (std::getline(m_in, line)) {
        if (!line.empty() && line.back() == '\r')
            line.pop_back();
        if (line.empty()) {
            if (!has_length)
                continue;
            std::string body(length, '\0');
            m_in.read(body.data(), static_cast<std::streamsize>(length));
            if (static_cast<std::size_t>(m_in.gcount()) != length)
                return std::nullopt;
            return body;
        }
        constexpr std::string_view header = "content-length:";
        if (line.size() > header.size()) {
            std::string lower = line.substr(0, header.size());
            for (auto& c : lower)
                c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
            if (lower == header) {
                length = static_cast<std::size_t>(std::strtoull(line.c_str() + header.size(), nullptr, 10));
                has_length = true;
            }
        }
    }
    return std::nullopt;
}

std::optional<Json> Transport::read() {
    while (true) {
        const auto raw = read_raw();
        if (!raw)
            return std::nullopt;
        std::string error;
        if (auto message = Json::parse(*raw, &error))
            return message;
        respond_error(Json {}, error_code::parse_error, "Parse error: " + error);
    }
}

void Transport::write(const Json& message) {
    const auto body = message.dump();
    std::lock_guard lock { m_write_lock };
    if (m_framing == Framing::ContentLength)
        m_out << "Content-Length: " << body.size() << "\r\n\r\n" << body;
    else
        m_out << body << "\n";
    m_out.flush();
}

void Transport::respond(const Json& id, Json result) {
    Json message = Json::object();
    message["jsonrpc"] = "2.0";
    message["id"] = id;
    message["result"] = std::move(result);
    write(message);
}

void Transport::respond_error(const Json& id, int code, std::string text) {
    Json message = Json::object();
    message["jsonrpc"] = "2.0";
    message["id"] = id;
    Json error = Json::object();
    error["code"] = code;
    error["message"] = std::move(text);
    message["error"] = std::move(error);
    write(message);
}

void Transport::notify(std::string method, Json params) {
    Json message = Json::object();
    message["jsonrpc"] = "2.0";
    message["method"] = std::move(method);
    message["params"] = std::move(params);
    write(message);
}

void Transport::request(const Json& id, std::string method, Json params) {
    Json message = Json::object();
    message["jsonrpc"] = "2.0";
    message["id"] = id;
    message["method"] = std::move(method);
    message["params"] = std::move(params);
    write(message);
}

void use_binary_stdio() {
#ifdef _WIN32
    _setmode(_fileno(stdin), _O_BINARY);
    _setmode(_fileno(stdout), _O_BINARY);
#endif
}

}
