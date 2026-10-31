#pragma once
#include "sc_sema/details/symbols.hpp"
#include <vector>
#include <optional>

namespace sc::ir {

/// @brief What the VM requires of a class, e.g., it must have certain members in a certain order.
/// Each requirement is only checked if it is set.
struct IntrinsicConstraint {
    SymSCClassName target; ///< The class the constraint is for.
    std::optional<OptionalSymSCClassName> super_class; ///< The superclass it must have, which may be none.
    std::optional<std::vector<SymSCIdentifier>> member_names; ///< The instance variables it must have, in order.
    std::optional<SymSCIdentifier> slot_def; ///< The slot type it must be declared with.
};

/// @brief The constraints for every class that has one.
using IntrinsicConstraints = std::unordered_map<SymSCClassName, IntrinsicConstraint, std::hash<Symbol>>;
}