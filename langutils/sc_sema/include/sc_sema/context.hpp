#pragma once

#include "sc_sema/details/class_library_sources.hpp"
#include "sc_sema/details/symbols.hpp"
#include "sc_sema/details/const_expr.hpp"
#include "sc_sema/module.hpp"

#include <cstddef>
#include <cstdint>
#include <memory>

namespace sc::ir {

struct SymbolTable;

/// @brief Everything the compilation shares: the symbols, constants, declared classes, and diagnostics.
/// Its layout is private to sc_sema, so it is only made with make_context() and read with the functions below.
struct Context;

/// @brief Deletes a Context, which is an incomplete type here.
struct ContextDeleter {
    void operator()(Context* ctx) const;
};
/// @brief Owns a Context.
using ContextPtr = std::unique_ptr<Context, ContextDeleter>;

/// @brief Makes the context for a compilation.
/// The symbol table, modules, and class library sources are referred to, not owned, so they must outlive the context.
/// @param symbols Where symbols are interned.
/// @param root_object The class every class inherits from.
/// @param modules The paths given to the compiler.
/// @param class_library The parsed files of the class library.
[[nodiscard]] ContextPtr make_context(SymbolTable& symbols, SymSCClassName root_object, ModuleTable& modules,
                                      ClassLibrarySources& class_library);

/// @brief The symbol table the context was made with.
[[nodiscard]] const SymbolTable& symbol_table(const Context& ctx);
/// @brief The constants found while compiling.
[[nodiscard]] const ConstantTable& constant_table(const Context& ctx);
/// @brief The `Array` class, which the compiler refers to by name. Found in the symbol table when the context is made.
[[nodiscard]] SymSCClassName array_class(const Context& ctx);
/// @brief The `Event` class, which the compiler refers to by name.
[[nodiscard]] SymSCClassName event_class(const Context& ctx);

/// @brief The `Interpreter` class, which the compiler refers to by name.
[[nodiscard]] SymSCClassName interpreter_class(const Context& ctx);
/// @brief The identifier `this`.
[[nodiscard]] SymSCIdentifier this_identifier(const Context& ctx);

/// @brief How many classes were declared without errors, counting each class and its meta class.
[[nodiscard]] std::size_t num_declared_classes(const Context& ctx);

/// @brief Adds a primitive's arguments from the VM's table, which a method calling it must match.
/// Keeps the existing one if already added. Call before compiling the class library.
/// @param ctx The context.
/// @param primitive The primitive.
/// @param num_arguments The number of arguments, which counts the receiver, as the VM does.
/// @param variadic_arguments Whether it accepts variadic arguments.
/// @param keyword_arguments Whether it accepts keyword arguments.
void add_primitive_signature(Context& ctx, SymSCPrimitive primitive, std::uint8_t num_arguments,
                             bool variadic_arguments, bool keyword_arguments);

}
