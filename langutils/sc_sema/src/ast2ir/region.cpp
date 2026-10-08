
#include "../ast2ir.hpp"
#include "sc_parser/indexes_typed.hpp"

namespace sc::ir::ast2ir {

Return Region::operator()(ast::ErrorIndex) { assert(false);}

Return Region::operator()(ast::AnyExprIndex expr) {
    return std::visit(Expr { ir_graph, ast_graph, ast_index }, ast_graph.index_to_variant(expr));
}

}

