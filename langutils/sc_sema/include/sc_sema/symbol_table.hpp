#pragma once

#include "sc_sema/details/symbols.hpp"
#include <array>
#include <atomic>
#include <cstdint>
#include <memory>
#include <string_view>
namespace sc::ir {

/// @brief Interns strings as Symbols, from many threads at once.
///
/// Each string is stored once, in an arena that never moves, and the maps are split into shards, each with its own
/// lock, so threads interning different strings rarely wait for each other. Looking a Symbol's string up takes no lock.
/// Symbols are numbered in the order they are first seen, which varies between runs when threaded, so nothing should be
/// ordered by them.
///
/// Each kind of symbol has its own getter. The same text interned as two kinds gives two different Symbols.
struct SymbolTable {
    /// @brief Interns a string as an untyped Symbol.
    [[nodiscard]] Symbol get(std::string_view s);

    /// @brief Interns a message name.
    [[nodiscard]] SymSCSelector get_selector(std::string_view s);
    /// @brief Interns a class name.
    [[nodiscard]] SymSCClassName get_class_name(std::string_view s);
    /// @brief Interns the name of the meta class of the class named @p s, e.g., `Meta_Object`.
    [[nodiscard]] SymSCClassName get_meta_class_name(std::string_view s);
    /// @brief The name of the meta class of a class.
    [[nodiscard]] SymSCClassName get_meta_class_name(SymSCClassName);


    /// @brief Interns a primitive name.
    [[nodiscard]] SymSCPrimitive get_primitive(std::string_view);

    /// @brief Interns a symbol literal.
    [[nodiscard]] SymSCSymbol get_symbol(std::string_view s);
    /// @brief Interns a variable, argument, or other name.
    [[nodiscard]] SymSCIdentifier get_identifier(std::string_view s);
    /// @brief Interns a string literal.
    [[nodiscard]] SymSCString get_string(std::string_view s);

    /// @brief Whether a class name is a meta class.
    /// Meta classes are named with a prefix, `Meta_Object`, which a class can't be named with.
    [[nodiscard]] bool is_meta(SymSCClassName n) const;

    SymbolTable();
    ~SymbolTable();
    SymbolTable(const SymbolTable&) = delete;
    SymbolTable& operator=(const SymbolTable&) = delete;

    /// @brief The string of a Symbol.
    /// The view stays valid while the table exists, even as other threads add symbols.
    [[nodiscard]] std::string_view operator()(Symbol i) const noexcept;

private:
    struct Shard;
    static constexpr std::size_t shard_count { 64 };
    std::unique_ptr<Shard[]> m_shards;

    // Each Symbol's string, in chunks that double in size, the first holding 2^first_chunk_bits, so a chunk never moves
    // once it is made, and reading doesn't need a lock. An entry is written before its Symbol is returned, so any
    // thread given the Symbol can read it. 32 bit Symbols need at most chunk_count chunks.
    static constexpr std::size_t first_chunk_bits { 10 };
    static constexpr std::size_t chunk_count { 33 - first_chunk_bits };
    [[nodiscard]] std::string_view& entry(std::uint32_t id);
    std::array<std::atomic<std::string_view*>, chunk_count> m_chunks {};
    std::atomic<std::uint32_t> m_count { 0 };
};
}
