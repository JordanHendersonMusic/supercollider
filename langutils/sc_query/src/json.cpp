#include "sc_query/json.hpp"

#include <cmath>
#include <cstdio>
#include <cstdlib>

namespace sc::query {

namespace {
const Json null_json {};
const Json::Array empty_array {};
const Json::Object empty_object {};

void append_utf8(std::string& out, std::uint32_t c) {
    if (c < 0x80) {
        out += static_cast<char>(c);
    } else if (c < 0x800) {
        out += static_cast<char>(0xC0 | (c >> 6));
        out += static_cast<char>(0x80 | (c & 0x3F));
    } else if (c < 0x10000) {
        out += static_cast<char>(0xE0 | (c >> 12));
        out += static_cast<char>(0x80 | ((c >> 6) & 0x3F));
        out += static_cast<char>(0x80 | (c & 0x3F));
    } else {
        out += static_cast<char>(0xF0 | (c >> 18));
        out += static_cast<char>(0x80 | ((c >> 12) & 0x3F));
        out += static_cast<char>(0x80 | ((c >> 6) & 0x3F));
        out += static_cast<char>(0x80 | (c & 0x3F));
    }
}

struct Parser {
    std::string_view text;
    std::size_t at { 0 };
    std::string error {};
    int depth { 0 };

    bool fail(const char* what) {
        if (error.empty())
            error = std::string { what } + " at offset " + std::to_string(at);
        return false;
    }
    void skip_space() {
        while (at < text.size() && (text[at] == ' ' || text[at] == '\t' || text[at] == '\n' || text[at] == '\r'))
            ++at;
    }
    bool literal(std::string_view word) {
        if (text.substr(at, word.size()) != word)
            return fail("unexpected text");
        at += word.size();
        return true;
    }
    bool hex4(std::uint32_t& out) {
        if (at + 4 > text.size())
            return fail("short unicode escape");
        out = 0;
        for (int i = 0; i < 4; ++i) {
            const char c = text[at++];
            out <<= 4;
            if (c >= '0' && c <= '9')
                out |= static_cast<std::uint32_t>(c - '0');
            else if (c >= 'a' && c <= 'f')
                out |= static_cast<std::uint32_t>(c - 'a' + 10);
            else if (c >= 'A' && c <= 'F')
                out |= static_cast<std::uint32_t>(c - 'A' + 10);
            else
                return fail("bad unicode escape");
        }
        return true;
    }
    bool string(std::string& out) {
        ++at; // the quote
        while (at < text.size()) {
            const char c = text[at++];
            if (c == '"')
                return true;
            if (c != '\\') {
                out += c;
                continue;
            }
            if (at >= text.size())
                break;
            const char e = text[at++];
            switch (e) {
            case '"':
            case '\\':
            case '/':
                out += e;
                break;
            case 'b':
                out += '\b';
                break;
            case 'f':
                out += '\f';
                break;
            case 'n':
                out += '\n';
                break;
            case 'r':
                out += '\r';
                break;
            case 't':
                out += '\t';
                break;
            case 'u': {
                std::uint32_t c1 = 0;
                if (!hex4(c1))
                    return false;
                if (c1 >= 0xD800 && c1 < 0xDC00 && text.substr(at, 2) == "\\u") {
                    at += 2;
                    std::uint32_t c2 = 0;
                    if (!hex4(c2))
                        return false;
                    if (c2 >= 0xDC00 && c2 < 0xE000)
                        c1 = 0x10000 + ((c1 - 0xD800) << 10) + (c2 - 0xDC00);
                    else
                        append_utf8(out, 0xFFFD), c1 = c2;
                }
                append_utf8(out, c1);
                break;
            }
            default:
                return fail("bad escape");
            }
        }
        return fail("unterminated string");
    }
    bool number(Json& out) {
        const auto start = at;
        bool is_double = false;
        if (at < text.size() && text[at] == '-')
            ++at;
        while (at < text.size()) {
            const char c = text[at];
            if (c >= '0' && c <= '9') {
                ++at;
            } else if (c == '.' || c == 'e' || c == 'E' || c == '+' || c == '-') {
                is_double = true;
                ++at;
            } else {
                break;
            }
        }
        const std::string digits { text.substr(start, at - start) };
        if (digits.empty() || digits == "-")
            return fail("bad number");
        char* end = nullptr;
        if (!is_double) {
            const long long v = std::strtoll(digits.c_str(), &end, 10);
            if (end == digits.c_str() + digits.size()) {
                out = Json { v };
                return true;
            }
        }
        const double d = std::strtod(digits.c_str(), &end);
        if (end != digits.c_str() + digits.size())
            return fail("bad number");
        out = Json { d };
        return true;
    }
    bool value(Json& out) {
        if (++depth > 512)
            return fail("nested too deeply");
        skip_space();
        if (at >= text.size())
            return fail("unexpected end");
        const char c = text[at];
        bool ok = true;
        if (c == '{') {
            ++at;
            out = Json::object();
            skip_space();
            if (at < text.size() && text[at] == '}') {
                ++at;
            } else {
                while (true) {
                    skip_space();
                    if (at >= text.size() || text[at] != '"')
                        return fail("expected a key");
                    std::string key;
                    if (!string(key))
                        return false;
                    skip_space();
                    if (at >= text.size() || text[at] != ':')
                        return fail("expected ':'");
                    ++at;
                    Json member;
                    if (!value(member))
                        return false;
                    out[key] = std::move(member);
                    skip_space();
                    if (at < text.size() && text[at] == ',') {
                        ++at;
                        continue;
                    }
                    if (at < text.size() && text[at] == '}') {
                        ++at;
                        break;
                    }
                    return fail("expected ',' or '}'");
                }
            }
        } else if (c == '[') {
            ++at;
            out = Json::array();
            skip_space();
            if (at < text.size() && text[at] == ']') {
                ++at;
            } else {
                while (true) {
                    Json element;
                    if (!value(element))
                        return false;
                    out.push_back(std::move(element));
                    skip_space();
                    if (at < text.size() && text[at] == ',') {
                        ++at;
                        continue;
                    }
                    if (at < text.size() && text[at] == ']') {
                        ++at;
                        break;
                    }
                    return fail("expected ',' or ']'");
                }
            }
        } else if (c == '"') {
            std::string s;
            ok = string(s);
            out = Json { std::move(s) };
        } else if (c == 't') {
            ok = literal("true");
            out = Json { true };
        } else if (c == 'f') {
            ok = literal("false");
            out = Json { false };
        } else if (c == 'n') {
            ok = literal("null");
            out = Json {};
        } else {
            ok = number(out);
        }
        --depth;
        return ok;
    }
};
}

const Json& Json::operator[](std::string_view key) const {
    for (const auto& [k, v] : m_object)
        if (k == key)
            return v;
    return null_json;
}

Json& Json::operator[](std::string_view key) {
    if (m_type == Type::Null)
        m_type = Type::Object;
    for (auto& [k, v] : m_object)
        if (k == key)
            return v;
    m_object.emplace_back(std::string { key }, Json {});
    return m_object.back().second;
}

const Json& Json::operator[](std::size_t i) const { return i < m_array.size() ? m_array[i] : null_json; }

bool Json::contains(std::string_view key) const {
    for (const auto& [k, v] : m_object)
        if (k == key)
            return true;
    return false;
}

void Json::push_back(Json value) {
    if (m_type == Type::Null)
        m_type = Type::Array;
    m_array.push_back(std::move(value));
}

std::size_t Json::size() const {
    if (m_type == Type::Array)
        return m_array.size();
    if (m_type == Type::Object)
        return m_object.size();
    return 0;
}

std::string Json::str(std::string fallback) const { return m_type == Type::String ? m_string : fallback; }

std::int64_t Json::integer(std::int64_t fallback) const {
    if (m_type == Type::Int)
        return m_int;
    if (m_type == Type::Double)
        return static_cast<std::int64_t>(m_double);
    return fallback;
}

double Json::number(double fallback) const {
    if (m_type == Type::Int)
        return static_cast<double>(m_int);
    if (m_type == Type::Double)
        return m_double;
    return fallback;
}

bool Json::boolean(bool fallback) const { return m_type == Type::Bool ? m_bool : fallback; }

const Json::Array& Json::items() const { return m_type == Type::Array ? m_array : empty_array; }

const Json::Object& Json::members() const { return m_type == Type::Object ? m_object : empty_object; }

void write_json_string(std::string& out, std::string_view s) {
    out += '"';
    for (const char c : s) {
        switch (c) {
        case '"':
            out += "\\\"";
            break;
        case '\\':
            out += "\\\\";
            break;
        case '\n':
            out += "\\n";
            break;
        case '\r':
            out += "\\r";
            break;
        case '\t':
            out += "\\t";
            break;
        default:
            if (static_cast<unsigned char>(c) < 0x20) {
                char escaped[8];
                std::snprintf(escaped, sizeof escaped, "\\u%04x", static_cast<unsigned>(c));
                out += escaped;
            } else {
                out += c;
            }
        }
    }
    out += '"';
}

void Json::dump(std::string& out) const {
    switch (m_type) {
    case Type::Null:
        out += "null";
        break;
    case Type::Bool:
        out += m_bool ? "true" : "false";
        break;
    case Type::Int:
        out += std::to_string(m_int);
        break;
    case Type::Double: {
        if (!std::isfinite(m_double)) {
            out += "null";
            break;
        }
        char buffer[32];
        std::snprintf(buffer, sizeof buffer, "%.17g", m_double);
        out += buffer;
        break;
    }
    case Type::String:
        write_json_string(out, m_string);
        break;
    case Type::Array: {
        out += '[';
        bool first = true;
        for (const auto& v : m_array) {
            if (!first)
                out += ',';
            first = false;
            v.dump(out);
        }
        out += ']';
        break;
    }
    case Type::Object: {
        out += '{';
        bool first = true;
        for (const auto& [k, v] : m_object) {
            if (!first)
                out += ',';
            first = false;
            write_json_string(out, k);
            out += ':';
            v.dump(out);
        }
        out += '}';
        break;
    }
    }
}

std::string Json::dump() const {
    std::string out;
    dump(out);
    return out;
}

std::optional<Json> Json::parse(std::string_view text, std::string* error) {
    Parser p { text };
    Json out;
    if (!p.value(out)) {
        if (error)
            *error = p.error;
        return std::nullopt;
    }
    p.skip_space();
    if (p.at != text.size()) {
        if (error)
            *error = "trailing text at offset " + std::to_string(p.at);
        return std::nullopt;
    }
    return out;
}

bool operator==(const Json& l, const Json& r) {
    if (l.is_number() && r.is_number())
        return l.number() == r.number();
    if (l.m_type != r.m_type)
        return false;
    switch (l.m_type) {
    case Json::Type::Null:
        return true;
    case Json::Type::Bool:
        return l.m_bool == r.m_bool;
    case Json::Type::String:
        return l.m_string == r.m_string;
    case Json::Type::Array:
        return l.m_array == r.m_array;
    case Json::Type::Object:
        return l.m_object == r.m_object;
    default:
        return false;
    }
}

}
