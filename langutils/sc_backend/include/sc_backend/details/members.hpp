#pragma once

#include "sc_sema/details/indexes.hpp"
#include "sc_sema/details/symbols.hpp"
#include "sc_util/strong_index.hpp"

#include <cstdint>
#include <optional>
#include <tuple>
#include <unordered_map>
#include <variant>

namespace sc::ir {
struct IRCompiler;
struct SymbolTable;
}

namespace sc::backend {

////////////////////////////////////////////////////////////////////////////////
// API
////////////////////////////////////////////////////////////////////////////////

/// @brief The slot of an instance variable in an object.
using InstanceSlotIndex = util::StrongIndex<std::uint32_t, struct InstanceSlot__>;
/// @brief The index of a class variable among all the class variables.
/// Numbered by meta class name, which the loader must follow.
using ClassVariableIndex = util::StrongIndex<std::uint32_t, struct ClassVariableIndex__>;

/// @brief Where a member is stored: an instance slot, a class variable, or, for a constant, nowhere as it is pushed.
using MemberLocation = std::variant<InstanceSlotIndex, ClassVariableIndex, ir::OptionalConstExprIndex>;
static_assert(sizeof(MemberLocation) == 8);

/// @brief Where every member of every compiled class is stored.
struct MemberTable {
    /// @brief Lays out the members of every class the compiler compiled.
    MemberTable(const ir::IRCompiler& compiler, const ir::SymbolTable& symbols);

    /// @brief Where a member is stored.
    /// @param owner The class that declares the member.
    /// @param name The member's name.
    /// @return The location, empty if the class has no such member.
    [[nodiscard]] std::optional<MemberLocation> find(ir::SymSCClassName owner, ir::SymSCIdentifier name) const;
    /// @brief The index of the first class variable of a meta class.
    [[nodiscard]] ClassVariableIndex class_variable_base(ir::SymSCClassName meta) const;
    /// @brief How many class variables there are in all.
    [[nodiscard]] std::uint32_t num_class_variables() const { return m_num_class_variables; }

private:
    using MemberLocationKey = std::tuple<ir::SymSCClassName, ir::SymSCIdentifier>;

    struct MemberLocationKeyHash {
        [[nodiscard]] std::size_t operator()(const MemberLocationKey& k) const {
            return std::hash<std::uint64_t> {}((static_cast<std::uint64_t>(*std::get<0>(k)) << 32) | *std::get<1>(k));
        }
    };
    [[nodiscard]] static MemberLocationKey key(ir::SymSCClassName owner, ir::SymSCIdentifier name) {
        return { owner, name };
    }

    std::unordered_map<MemberLocationKey, MemberLocation, MemberLocationKeyHash> m_members {};
    std::unordered_map<ir::SymSCClassName, ClassVariableIndex, std::hash<ir::Symbol>> m_class_variable_bases {};
    std::uint32_t m_num_class_variables {};
};

}
