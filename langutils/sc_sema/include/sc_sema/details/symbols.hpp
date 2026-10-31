#pragma once
#include "sc_util/type_set_index.hpp"
#include <cstdint>

namespace sc::ir {

/// @brief The kinds of symbol a SymbolTable interns.
enum struct SymbolTypes {
    SCSymbol, ///< A symbol literal.
    SCString, ///< A string literal.
    SCClassName, ///< A class or meta class name.
    SCNamedIdentifier, ///< A variable, argument, or other name.
    SCSelector, ///< A message name.
    SCPrimitive, ///< A primitive name.
};

/// @brief The index types of the symbols.
using SymbolDef = util::typed_index::QuicklyDefineTypesFromSpec<
    util::typed_index::Spec<std::uint32_t, SymbolTypes, struct Symbol____>>;

/// @brief A symbol of any kind.
using Symbol = SymbolDef::Index;

/// @name Symbols of one kind
/// Each converts to Symbol. The `Optional` forms may be empty.
/// @{
using SymSCSymbol = SymbolDef::TypedIndex<SymbolTypes::SCSymbol>;
using OptionalSymSCSymbol = SymbolDef::OptionalTypedIndex<SymbolTypes::SCSymbol>;
using SymSCString = SymbolDef::TypedIndex<SymbolTypes::SCString>;
using SymSCClassName = SymbolDef::TypedIndex<SymbolTypes::SCClassName>;
using OptionalSymSCClassName = SymbolDef::OptionalTypedIndex<SymbolTypes::SCClassName>;

using SymSCIdentifier = SymbolDef::TypedIndex<SymbolTypes::SCNamedIdentifier>;
using OptionalSymSCIdentifier = SymbolDef::OptionalTypedIndex<SymbolTypes::SCNamedIdentifier>;
using SymSCSelector = SymbolDef::TypedIndex<SymbolTypes::SCSelector>;
using OptionalSymSCSelector = SymbolDef::OptionalTypedIndex<SymbolTypes::SCSelector>;
using SymSCPrimitive = SymbolDef::TypedIndex<SymbolTypes::SCPrimitive>;
using OptionalSymSCPrimitive = SymbolDef::OptionalTypedIndex<SymbolTypes::SCPrimitive>;
/// @}
}
