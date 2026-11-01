#pragma once

/// @file
/// @brief A small JSON value, enough for JSON-RPC, the LSP and MCP. Objects keep their insertion order.

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace sc::query {

////////////////////////////////////////////////////////////////////////////////
// API
////////////////////////////////////////////////////////////////////////////////

/// @brief A JSON value: null, a boolean, a number, a string, an array or an object.
class Json {
public:
    enum struct Type : std::uint8_t { Null, Bool, Int, Double, String, Array, Object };
    using Array = std::vector<Json>;
    using Member = std::pair<std::string, Json>;
    using Object = std::vector<Member>;

    Json() = default;
    Json(std::nullptr_t) {}
    Json(bool b): m_type(Type::Bool), m_bool(b) {}
    Json(int i): m_type(Type::Int), m_int(i) {}
    Json(unsigned i): m_type(Type::Int), m_int(i) {}
    Json(long i): m_type(Type::Int), m_int(i) {}
    Json(unsigned long i): m_type(Type::Int), m_int(static_cast<std::int64_t>(i)) {}
    Json(long long i): m_type(Type::Int), m_int(i) {}
    Json(unsigned long long i): m_type(Type::Int), m_int(static_cast<std::int64_t>(i)) {}
    Json(double d): m_type(Type::Double), m_double(d) {}
    Json(const char* s): m_type(Type::String), m_string(s) {}
    Json(std::string s): m_type(Type::String), m_string(std::move(s)) {}
    Json(std::string_view s): m_type(Type::String), m_string(s) {}
    Json(Array a): m_type(Type::Array), m_array(std::move(a)) {}
    Json(Object o): m_type(Type::Object), m_object(std::move(o)) {}

    /// @brief An empty object.
    [[nodiscard]] static Json object() { return Json { Object {} }; }
    /// @brief An empty array.
    [[nodiscard]] static Json array() { return Json { Array {} }; }

    [[nodiscard]] Type type() const { return m_type; }
    [[nodiscard]] bool is_null() const { return m_type == Type::Null; }
    [[nodiscard]] bool is_bool() const { return m_type == Type::Bool; }
    [[nodiscard]] bool is_number() const { return m_type == Type::Int || m_type == Type::Double; }
    [[nodiscard]] bool is_string() const { return m_type == Type::String; }
    [[nodiscard]] bool is_array() const { return m_type == Type::Array; }
    [[nodiscard]] bool is_object() const { return m_type == Type::Object; }

    /// @brief The member named @p key, or null if this isn't an object or has no such member.
    [[nodiscard]] const Json& operator[](std::string_view key) const;
    /// @brief The member named @p key, added if missing. A null becomes an empty object first.
    Json& operator[](std::string_view key);
    /// @brief The element at @p i, or null if this isn't an array or is too short.
    [[nodiscard]] const Json& operator[](std::size_t i) const;
    /// @brief Whether this is an object with a member named @p key.
    [[nodiscard]] bool contains(std::string_view key) const;

    /// @brief Appends to an array. A null becomes an empty array first.
    void push_back(Json value);
    /// @brief The number of elements or members, zero for anything else.
    [[nodiscard]] std::size_t size() const;

    /// @brief The string, or @p fallback if this isn't one.
    [[nodiscard]] std::string str(std::string fallback = {}) const;
    /// @brief The number as an integer, or @p fallback if this isn't a number.
    [[nodiscard]] std::int64_t integer(std::int64_t fallback = 0) const;
    /// @brief The number, or @p fallback if this isn't a number.
    [[nodiscard]] double number(double fallback = 0) const;
    /// @brief The boolean, or @p fallback if this isn't one.
    [[nodiscard]] bool boolean(bool fallback = false) const;

    /// @brief The elements, empty if this isn't an array.
    [[nodiscard]] const Array& items() const;
    /// @brief The members, empty if this isn't an object.
    [[nodiscard]] const Object& members() const;

    /// @brief Writes the value on one line.
    [[nodiscard]] std::string dump() const;
    void dump(std::string& out) const;

    /// @brief Parses a JSON text.
    /// @param text The text.
    /// @param error Set to what went wrong, when it fails.
    /// @return The value, or nothing if the text isn't valid JSON.
    [[nodiscard]] static std::optional<Json> parse(std::string_view text, std::string* error = nullptr);

    friend bool operator==(const Json& l, const Json& r);
    friend bool operator!=(const Json& l, const Json& r) { return !(l == r); }

private:
    Type m_type { Type::Null };
    bool m_bool { false };
    std::int64_t m_int { 0 };
    double m_double { 0 };
    std::string m_string {};
    Array m_array {};
    Object m_object {};
};

/// @brief Writes @p s as a quoted JSON string to @p out.
void write_json_string(std::string& out, std::string_view s);

}
