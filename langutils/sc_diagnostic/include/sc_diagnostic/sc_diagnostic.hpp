#pragma once

#include "sc_lexer/text_info.hpp"
#include "sc_lexer/text_location.hpp"

#include <ostream>
#include <memory>
#include <vector>

namespace sc::diag {
enum struct Severity { Error, Warning };

enum struct DiagnosticKind {
    UnexpectedToken,
    MissingSemiColonBetweenRegions,
};


class Diagnostic {
public:
    struct Location {
        std::shared_ptr<const sc::lex::TextInfo> text_info;
        sc::lex::SourceCodeRange range;
    };

    [[nodiscard]] static Diagnostic unexpectedToken(Diagnostic::Location loc, std::string expected, const char* received);
    [[nodiscard]] static Diagnostic regionMissingSemi(Diagnostic::Location loc);


    Diagnostic() = delete;
    Diagnostic(Diagnostic&&) noexcept = default;
    Diagnostic(const Diagnostic&) = default;
    Diagnostic& operator=(Diagnostic&&) noexcept = default;
    Diagnostic& operator=(const Diagnostic&) = default;
    ~Diagnostic() = default;

    [[nodiscard]] Location location() const;
    [[nodiscard]] Severity severity() const;
    [[nodiscard]] bool fatal() const;
    [[nodiscard]] const std::string& message() const;
    [[nodiscard]] const std::vector<Location>& extra_locations() const;
    [[nodiscard]] bool has_extra_locations() const;
    [[nodiscard]] const char* name() const;

    friend std::ostream& operator<<(std::ostream& s, const Diagnostic& d);

private:
    Diagnostic(const char* name, Location location, Severity severity, std::string message,
               std::vector<Location> extra_locations = { }) noexcept;
    Location m_location;
    std::vector<Location> m_extra_locations;
    std::string m_message;
    const char* m_name;
    Severity m_severity;
};


inline std::ostream& operator<<(std::ostream& s, const Diagnostic& d) {
    //      const auto [ptr, sz] = d.m_location.text_info->read(d.m_location.range);
    const auto& str = d.m_location.text_info->source.as_string();
    s << d.name() << '\n';
    s << str << '\n';
    s << d.message() << '\n' << '\n' << std::endl;
    s.flush();

    return s;
}

}
