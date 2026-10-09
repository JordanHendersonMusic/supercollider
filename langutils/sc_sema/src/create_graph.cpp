#include "sc_sema/create_graph.hpp"
#include "sc_parser/ast.hpp"
#include "sc_sema/ast_location.hpp"
#include "sc_sema/symbols.hpp"

#include "sc_parser/indexes_typed.hpp"
#include "sc_sema/ir.hpp"
#include <unordered_map>

namespace sc::ir {

const static std::string meta_prefix = "META_";

struct CommonClasses {
    SymSCClassName abstract_object, object, class_;
};

void declare_classes(const ast::ASTGraph& ast_graph, ASTGraphIndex ast_id, Context& context,
                     const CommonClasses& common_classes, ast::ClassIndex class_i) {
    const auto [class_name_i, backing_slot, super_name_i, members, methods] = ast_graph.children(class_i);

    const auto class_name_str = std::string { ast_graph.string(class_name_i) };
    const auto class_name = context.symbol_table.get<SymSCClassName>(class_name_str);

    const bool is_asb_obj = class_name == common_classes.abstract_object;

    if (auto loc = context.class_declarations.with_decl(class_name, [](ClassDeclaration& decl) {
            decl.poisoned = true;
            return decl.location;
        })) {
        const auto& other_ast = context.ast_of_class_library[*loc->graph_index].first;

        context.diagnostics( //
            Diagnostic::duplicate_class( //
                other_ast.text_info(), other_ast.location(loc->node_index), ast_graph.text_info(),
                ast_graph.location(class_i)) //
        );
        return;
    }

    // Create decl
    ClassDeclaration decl { { ast_id, ast::Index { *class_i } }, class_name };
    if (auto super = ast_graph.present(super_name_i)) {
        decl.super = context.symbol_table.get<SymSCClassName>(std::string { ast_graph.string(super_name_i) });
    } else if (is_asb_obj) {
        // do nothing
    } else {
        decl.super = common_classes.object;
    }

    // Create meta decl
    auto meta_class_name_str = meta_prefix + class_name_str;
    const auto meta_class_name = context.symbol_table.get<SymSCClassName>(std::move(meta_class_name_str));

    decl.meta_class = meta_class_name;

    ClassDeclaration meta_decl { { ast_id, ast::Index { *class_i } }, meta_class_name };

    // aka, if this is not abstract object
    if (decl.super) {
        std::string super_meta_class_name = meta_prefix + std::string { context.symbol_table(*decl.super) };
        meta_decl.super = context.symbol_table.get<SymSCClassName>(std::move(super_meta_class_name));
    } else {
        // META_AbstractObject inherits from Class
        meta_decl.super = common_classes.class_;
    }

    context.class_declarations.register_class_declaration(std::move(decl));
    context.class_declarations.register_class_declaration(std::move(meta_decl));
}


// clang-format off
IRCompiler::IRCompiler(
            std::unordered_map<SymSCClassName, IntrinsicConstraints, std::hash<Symbol>> intrinsic_constraints, 
            std::vector<GraphAndIndex> class_lib, 
            size_t num_threads)
        : context(std::move(class_lib)) 
{

    ////////////////////////////////////////////////////////////////////////////////
    // This mutates the context member.
    ////////////////////////////////////////////////////////////////////////////////

    (void)intrinsic_constraints;


    // clang-format on
    (void)num_threads; // Later we should use a thread pool.

    if (m_compile_has_been_called) {
        // slowest memory order is fine here. This shouldn't happen
        assert(false);
        return;
    }
    m_compile_has_been_called = true;

    const auto abs_obj_sym = context.symbol_table.get<SymSCClassName>("AbstractObject");
    const auto obj_sym = context.symbol_table.get<SymSCClassName>("Object");
    const auto class_sym = context.symbol_table.get<SymSCClassName>("Class");
    const CommonClasses common_classes { abs_obj_sym, obj_sym, class_sym };

    // Step 1: Declare all classes, checking intrinsic classes are correct

    size_t i { 0 };
    for (const auto& p : context.ast_of_class_library) {
        const auto& [ast_graph, root_index] = p;

        const auto classes_or_extensions = ast_graph.children(root_index);

        for (auto child { classes_or_extensions.iter() }; child; ++child) {
            if (const auto class_i = ast_graph.as_a<ast::ClassIndex>(*child)) {
                declare_classes(ast_graph, ASTGraphIndex::from(i), context, common_classes, *class_i);
            }
        }

        i += 1;
    }


    // Step 2: Check hierarchy makes sense.

    // Step 3: Compile all ast class nodes.
    // Step 4: Compile all ast class extension nodes.


    // TOOD: this should use a thread pool in future.
    // // The order of the irgraphs does not matter.
    // size_t i { };
    // for (const auto& ast : m_class_lib_ast) {
    //     if (ast) {
    //         if (const auto root = ast.root())
    //             insert_ir(create_graph(ast, *root, ASTGraphIndex::from(i)));
    //     }

    //     i += 1;
    // }
}

void IRCompiler::insert_class_ir(SymSCClassName name, IRGraph&& graph, SCClass_I class_index) {
    std::scoped_lock lock { m_ir_graphs_lock };
    m_classes.insert({ name, ClassData { name, std::move(graph), class_index } });
}

void IRCompiler::poison_class(SymSCClassName name) {
    {
        std::scoped_lock lock { m_ir_graphs_lock };
        m_classes.find(name)->second.poisoned = true;
    }
    context.class_declarations.with_decl(name, [](ClassDeclaration& decl) {
        decl.poisoned = true;
        return;
    });
}
}
