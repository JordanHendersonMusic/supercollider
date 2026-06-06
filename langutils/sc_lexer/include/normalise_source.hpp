#pragma once

#include <string>

namespace sc::lex {

// Removes platform specific features (such as newline encoding).
struct NormalisedSource {
    NormalisedSource(const char* source, std::size_t sz) { normalise_string(source, sz, s); }
    NormalisedSource(const std::string& raw) { normalise_string(raw.c_str(), raw.size(), s); }


    NormalisedSource(NormalisedSource&&) noexcept = default;
    NormalisedSource(const NormalisedSource&) = default;
    NormalisedSource& operator=(NormalisedSource&&) noexcept = default;
    NormalisedSource& operator=(const NormalisedSource&) = default;
    ~NormalisedSource() = default;

    [[nodiscard]] const std::string& as_string() const& { return s; }
    [[nodiscard]] std::string steal_string() && { return std::move(s); }

private:
    std::string s {};
    static void normalise_string(const char* in, std::size_t sz, std::string& out) {
        out.reserve(sz);
        for (std::size_t i { 0 }; i < sz; ++i) {
            switch (in[i]) {
            case '\r': {
                if (i + 1 < sz && in[i + 1] == '\n') {
                    i += 1; // skip \n too
                    out.push_back('\n');
                } else {
                    out.push_back('\n');
                }

            } break;
            case '\v':
            case '\f':
                out.push_back('\n');
                break;
            default:
                out.push_back(in[i]);
                break;
            }
        }
    }
};


}
