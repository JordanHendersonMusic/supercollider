#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "symbol_table.hpp"

namespace sc::ir {


// struct ASTLocation {
//     std::uint32_t text_index;
//     std::uint32_t ast_node_index;
// };

// template <typename T> struct Locatable {
//     T t;
//     ASTLocation location;
// };


// struct ConstExprIndex {
//     std::size_t v;
// };

// struct NodeIndex {
//     std::size_t v;
// };

// struct ChildrenView {
//     // This makes things easy to serialize!
//     NodeIndex start;
//     std::size_t length;
// };

// enum struct SlotDef { None, Int, Float, Symbol };

// struct Class {
//     Locatable<std::string> name;
//     Locatable<SlotDef> slot_def;

//     ChildrenView class_members, instance_members;
//     ChildrenView class_methods, instance_methods;

//     ChildrenView sub_classes;
// };

// struct Method {
//     Locatable<std::string> name;
//     ChildrenView arguments;
//     std::optional<Locatable<std::string>> var_args_name, keyword_var_args_name;
//     std::optional<Locatable<std::string>> primitive_name;
//     ChildrenView body;
// };

// struct Function {
//     std::optional<Locatable<std::string>> name;
//     ChildrenView arguments;
//     std::optional<Locatable<std::string>> var_args_name, keyword_var_args_name;
//     ChildrenView body;
// };

// struct Member {
//     Locatable<std::string> name;
//     ChildrenView default_value;
//     bool make_setter;
//     bool make_getter;
//     bool constant;
// };

// struct PositionalArgument {
//     Locatable<std::string> name;
//     bool preserve_nil;
//     ChildrenView default_value;
// };


// struct Literal {
//     // This is shared in the const_expr map entry.
//     std::shared_ptr<ConstExpr> value;
// };

// struct Message {
//     Locatable<std::string> selector;
//     ChildrenView positional_args;
//     ChildrenView keyword_args;
//     ChildrenView variadic_args;
// };

// struct VariableDeclare {
//     Locatable<std::string> name;
//     ChildrenView default_value;
// };

// struct ReturnCaret {
//     ChildrenView expr;
// };

// struct VariableAssign {
//     Locatable<std::string> name;
//     ChildrenView expr;
// };

// struct MemberAssign {
//     Locatable<std::string> name;
//     ChildrenView expr;
// };


// struct IRContainer {
//     std::vector<std::size_t> children_spans;

//     std::unordered_map<std::string, NodeIndex> class_lookup;
//     using Nodes = std::variant<Class, Method, Literal>;


//     std::vector<Nodes> nodes;
//     std::vector<ASTLocation> location;

//     std::unordered_map<std::size_t, std::shared_ptr<ConstExpr>> const_expr;
// };
}
