#pragma once

#include "sc_sema/details/indexes.hpp"
#include "sc_util/details/typed_graph_node.hpp"

namespace sc::ir {

/// @brief `this`, `super`, and the pseudo variables, e.g., `thisProcess`.
/// These aren't variables: the language sets them, nothing declares them, and they can't be assigned.
struct SCSpecial : sc::util::typed_graph::TerminalNode<SCSpecial_I> {
    static constexpr auto type_name { "SCSpecial" };
    /// @brief Which special value.
    enum struct Kind {
        This, ///< `this`.
        /// `this`, but a message sent to it is looked up from the superclass of the class the method is defined in.
        Super,
        Process, ///< `thisProcess`.
        Thread, ///< `thisThread`.
        Method, ///< `thisMethod`.
        FunctionDef, ///< `thisFunctionDef`.
        Function, ///< `thisFunction`.
    } kind;
    SCSpecial(Kind k): kind(k) {}
};


/// @brief How the elements of a class's instances are stored: not indexed, or as slots of the given type.
enum struct MemoryLayout { NotIndexed, Slot, Double, Float, Int32, Int16, Int8, Char, Symbol };


}
