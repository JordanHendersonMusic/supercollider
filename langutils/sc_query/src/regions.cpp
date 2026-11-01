#include "sc_query/queries.hpp"

#include "sc_parser/ast.hpp"

namespace sc::query {

std::vector<Range> regions(const Document& document) {
    std::vector<Range> out;
    const auto& ast = document.ast();
    if (document.kind() != DocumentKind::Interpreted || !ast)
        return out;
    const auto root = ast->root();
    if (!root)
        return out;
    const auto* list = std::get_if<ast::RegionListIndex>(&*root);
    if (!list)
        return out;
    for (auto it = ast->children(*list).iter(); it; ++it)
        out.push_back(to_range(ast->location({ *ast::RegionIndex { *it } })));
    return out;
}

}
