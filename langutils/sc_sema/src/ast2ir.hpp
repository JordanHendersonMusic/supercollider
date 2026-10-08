#pragma once

#include "sc_parser/indexes_typed.hpp"
#include "sc_parser/ast.hpp"
#include "sc_sema/const_expr.hpp"
#include "sc_sema/ir.hpp"
#include "sc_sema/type_info.hpp"

namespace sc::ir::ast2ir {

////////////////////////////////////////////////////////////////////////////////



struct Return {
    constexpr Return(NodeIndex i) : ir_node(i) {}
    constexpr Return(NodeIndex i, TypeInfo info) : ir_node(i), type(info) {}
    constexpr Return(NodeIndex i, ConstExprIndex expr_i, TypeInfo info) : ir_node(i), const_index(expr_i), type(info) {}

    NodeIndex ir_node;
    std::optional<ConstExprIndex> const_index{};
    std::optional<TypeInfo> type{};
};

////////////////////////////////////////////////////////////////////////////////
struct Context {
    
    IRGraph& ir_graph;
    const ast::ASTGraph& ast_graph;
    ASTGraphIndex ast_index;
};
////////////////////////////////////////////////////////////////////////////////

struct Base {
    Base(IRGraph& ir, const ast::ASTGraph& ast, ASTGraphIndex ast_index) : ir_graph(ir), ast_graph(ast), ast_index(ast_index) {}
    IRGraph& ir_graph;
    const ast::ASTGraph& ast_graph;
    ASTGraphIndex ast_index;
};

////////////////////////////////////////////////////////////////////////////////

struct Region : private Base {
    using Base::Base;
    Return operator()(ast::ErrorIndex);
    Return operator()(ast::AnyExprIndex expr);
};

struct Expr : private Base {
    using Base::Base;
    Return operator()(ast::IntLitIndex i);
    Return operator()(ast::FloatLitIndex i);
    template<typename T>
    Return operator()(T) {};
};


}
