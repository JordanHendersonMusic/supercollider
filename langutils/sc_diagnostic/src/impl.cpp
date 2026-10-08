#include "sc_diagnostic/sc_diagnostic.hpp"


namespace sc::diag {

Diagnostic::Diagnostic(const char* name, Location location, Severity severity, std::string message,
                       std::vector<Location> extra_locations) noexcept:
    m_location(location),
    m_extra_locations(std::move(extra_locations)),
    m_message(std::move(message)),
    m_name(name),
    m_severity(severity) { }

Diagnostic::Location Diagnostic::location() const { return m_location; }

Severity Diagnostic::severity() const { return m_severity; }

bool Diagnostic::fatal() const { return m_severity == Severity::Error; }

[[nodiscard]] const std::string& Diagnostic::message() const { return m_message; }

[[nodiscard]] const std::vector<Diagnostic::Location>& Diagnostic::extra_locations() const { return m_extra_locations; }

[[nodiscard]] bool Diagnostic::has_extra_locations() const { return m_extra_locations.size() != 0; }

[[nodiscard]] const char* Diagnostic::name() const { return m_name; }

////////////////////////////////////////////////////////////////////////////////

Diagnostic Diagnostic::unexpectedToken(Diagnostic::Location loc, std::string expected, const char* received) {
    return {
        "Unexpected Token.",
        std::move(loc),
        Severity::Error,
        std::string{"Expected: "}  + std::move(expected) + " but received " + received,
        {}
    };
 };

Diagnostic Diagnostic::regionMissingSemi(Diagnostic::Location loc){
    return {
        "Region missing semi-colon.",
        std::move(loc),
        Severity::Warning,
        {},
        {}
    };
}
}

