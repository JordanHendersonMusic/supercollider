#pragma once

#include "sc_compiler/details/compiled_function.hpp"

#include "sc_source/text_info.hpp"

#include <memory>
#include <optional>
#include <variant>
#include <vector>

namespace sc::diag {
class Diagnostic;
}

namespace sc::compiler {

// Plain data: names are the library's symbols and values its constants, read through the library.

/// @brief A variable or constant of a compiled class.
struct CompiledMember {
    ir::SymSCIdentifier name; ///< The member's name.
    ir::OptionalConstExprIndex value; ///< Its value, nil without a value.
};
static_assert(sizeof(CompiledMember) == 8);

/// @brief A compiled class: the data the VM needs to create it. Plain data: names are the library's symbols and values
/// its constants, read through the library.
struct CompiledClass {
    ir::SymSCClassName name; ///< The class's name.
    ir::OptionalSymSCClassName superclass; ///< Its superclass, none for the root.
    ir::MemoryLayout memory_layout; ///< How the elements of its instances are stored.
    /// Begins at the class's name.
    ir::SourceLocation location;
    /// A class's, the inherited ones first. A meta class's are Class's, which the VM gives it.
    std::vector<CompiledMember> instance_variables;
    /// A meta class's own, which the VM keeps in its class, numbered from first_class_variable.
    std::optional<backend::ClassVariableIndex> first_class_variable;
    std::vector<CompiledMember> class_variables; ///< The class variables the class declares.
    std::vector<CompiledMember> constants; ///< The constants the class declares.
};

/// @brief A compiled method.
struct CompiledMethod {
    ir::SymSCSelector selector; ///< The method's name.
    backend::lir::FunctionIndex function; ///< Its code, in CompiledGraph::functions.
    ir::OptionalSymSCPrimitive primitive; ///< The primitive it calls, if any.
};
static_assert(sizeof(CompiledMethod) == 12);

/// @brief A compiled getter or setter. Its location is the member's name.
struct CompiledAccessor {
    ir::SymSCSelector selector; ///< The name of the getter or setter.
    ir::SourceLocation location; ///< The member's name.
    backend::OptionalMethodShortcut shortcut; ///< What it does.
};
static_assert(sizeof(CompiledAccessor) == 44);

/// @brief Compiled interpreted code.
struct CompiledInterpreted {
    /// A function for each region, in order.
    std::vector<backend::lir::FunctionIndex> functions;
};

/// @brief The result of compiling one class, or one piece of interpreted code.
struct CompiledGraph {
    std::variant<CompiledClass, CompiledInterpreted> unit; ///< What was compiled.
    /// The text the locations refer to.
    util::IndexedVector<ir::SourceIndex, std::shared_ptr<const src::TextInfo>> sources;
    util::IndexedVector<backend::lir::FunctionIndex, CompiledFunction> functions; ///< Every function.
    std::vector<CompiledMethod> methods; ///< The methods of a class.
    std::vector<CompiledAccessor> accessors; ///< The getters and setters of a class.
    /// What was found by the backend: the frames', then the emitter's.
    std::vector<diag::Diagnostic> diagnostics;
};
}
