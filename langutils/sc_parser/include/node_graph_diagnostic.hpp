#pragma once

#include "text_location.hpp"
#include "text_info.hpp"
#include <ostream>
#include <string>
#include <memory>
#include <vector>

namespace sc::parser::graph {

class Diagnostic {
public:
    enum struct Severity { Warning, Error };

    struct Location {
        std::shared_ptr<const TextInfo> text_info;
        sc::lex::SourceCodeRange range;
    };

    Diagnostic(const char* name, Location location, Severity severity, std::string message,
               std::vector<Location> extra_locations = {}) noexcept:
        m_location(location),
        m_extra_locations(std::move(extra_locations)),
        m_message(std::move(message)),
        m_name(name),
        m_severity(severity) {}
    Diagnostic() = delete;
    Diagnostic(Diagnostic&&) noexcept = default;
    Diagnostic(const Diagnostic&) = default;
    Diagnostic& operator=(Diagnostic&&) noexcept = default;
    Diagnostic& operator=(const Diagnostic&) = default;
    ~Diagnostic() = default;

    [[nodiscard]] Location location() const { return m_location; }
    [[nodiscard]] Severity severity() const { return m_severity; }
    [[nodiscard]] bool fatal() const { return m_severity == Severity::Error; }
    [[nodiscard]] const std::string& message() const { return m_message; }
    [[nodiscard]] const std::vector<Location>& extra_locations() const { return m_extra_locations; }
    [[nodiscard]] bool has_extra_locations() const { return m_extra_locations.size() != 0; }
    [[nodiscard]] const char* name() const { return m_name; }

    friend std::ostream& operator<<(std::ostream& s, const Diagnostic& d) {
        const auto [ptr, sz] = d.m_location.text_info->read(d.m_location.range);
        const auto& str = d.m_location.text_info->source.as_string();
        s << d.name() << '\n';
        s << str << '\n';
        s << d.message() << '\n' << '\n' << std::endl;
        s.flush();

        return s;
    }

private:
    Location m_location;
    std::vector<Location> m_extra_locations;
    std::string m_message;
    const char* m_name;
    Severity m_severity;
};


}
