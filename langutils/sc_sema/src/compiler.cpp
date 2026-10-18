#include "sc_diagnostic/sc_diagnostic.hpp"

#include "sc_parser/ast.hpp"
#include "sc_parser/indexes_typed.hpp"

#include "sc_sema/compiler.hpp"
#include "sc_sema/ast_location.hpp"
#include "sc_sema/const_expr.hpp"
#include "sc_sema/decls.hpp"
#include "sc_sema/symbol_table.hpp"
#include "sc_sema/symbols.hpp"
#include "sc_sema/ir.hpp"

#include "ast2ir.hpp"

#include <memory>
#include <unordered_map>

namespace sc::ir {

CommonClasses symbol_table_and_common_classes(SymbolTable& sym_table, SymSCClassName root_object) {
    const auto abs_obj_sym = sym_table.get_class_name("AbstractObject");
    const auto obj_sym = sym_table.get_class_name("Object");
    const auto class_sym = sym_table.get_class_name("Class");
    const auto string_sym = sym_table.get_class_name("String");
    const auto true_sym = sym_table.get_class_name("True");
    const auto false_sym = sym_table.get_class_name("False");
    const auto nil_sym = sym_table.get_class_name("Nil");
    const auto float_sym = sym_table.get_class_name("Float");
    const auto int_sym = sym_table.get_class_name("Integer");
    const auto char_sym = sym_table.get_class_name("Char");
    const auto sym_sym = sym_table.get_class_name("Symbol");
    const auto array_sym = sym_table.get_class_name("Array");
    const auto event_sym = sym_table.get_class_name("Event");

    return CommonClasses {
        //
        root_object,
        obj_sym, //
        class_sym, //
        string_sym, //
        true_sym, //
        false_sym, //
        nil_sym, //
        float_sym, //
        int_sym, //
        char_sym, //
        sym_sym, //
        array_sym, //
        event_sym //
    };
}

// Checks the constraint.
// As this mutates the class declaration, it is only thread safe if no other class mutates the same decl,
//  this is relatively easy to enforce as the contraint is const.
[[nodiscard]] std::vector<sc::diag::Diagnostic> check_intrinsic_constraint(const IntrinsicConstraint& c, Context& cxt,
                                                                           ClassDeclaration& decl) {
    assert(decl.name() == c.target);

    std::vector<sc::diag::Diagnostic> out;

    // Check super class
    [&]() {
        // both abscent
        if (!c.super_class && !decl.super())
            return;

        if (c.super_class && decl.super()) {
            // both present and equal
            if (*c.super_class == *decl.super()->first)
                return;

            // present, not equal
            const auto g = decl.ast_graph();
            out.push_back(sc::diag::Diagnostic::todo(g->text_info(), g->location(decl.super()->second)));
            return;
        }
        if (c.super_class && !decl.super()) {
            // Should have one, but doesn't
            const auto g = decl.ast_graph();
            out.push_back(sc::diag::Diagnostic::todo(g->text_info(), g->location(decl.name_index())));
        } else {
            // Shouldn't have one, but does.
            const auto g = decl.ast_graph();
            out.push_back(sc::diag::Diagnostic::todo(g->text_info(), g->location(decl.super()->second)));
        }
    }();

    // check slot def
    [&]() {
        if (!c.slot_def && !decl.slot_def())
            return;
        else if (c.slot_def && !decl.slot_def()) {
            // Should have a slot def, but doesn't
            const auto g = decl.ast_graph();
            out.push_back(sc::diag::Diagnostic::todo(g->text_info(), g->location(decl.name_index())));
            return;
        } else if (!c.slot_def && decl.slot_def()) {
            // Should not have one, but does
            const auto g = decl.ast_graph();
            out.push_back(sc::diag::Diagnostic::todo(g->text_info(), g->location(decl.slot_def()->second)));
            return;
        } else if (*c.slot_def != decl.slot_def()->first) {
            // Not equal
            const auto g = decl.ast_graph();
            out.push_back(sc::diag::Diagnostic::todo(g->text_info(), g->location(decl.slot_def()->second)));
            return;
        }
    }();

    // Check members

    // deal with empty requirement
    [&]() {
        // Expected both to be empty
        if (c.member_names.empty() && decl.member_order().empty())
            return;

        else if (c.member_names.empty() && !decl.member_order().empty()) {
            // Shouldn't be any.
            const auto first_name = decl.member_order().front();
            const auto& fnd = *decl.find_member(first_name);
            const auto g = decl.ast_graph();
            out.push_back(sc::diag::Diagnostic::todo(g->text_info(), g->location(fnd.name_index)));
            return;
        }
        // member_names is not empty.

        if (decl.member_order().empty()) {
            // Didn't provide any, uses class name location
            const auto g = decl.ast_graph();
            out.push_back(sc::diag::Diagnostic::todo(g->text_info(), g->location(decl.name_index())));
            return;
        }

        // both have something.

        if (decl.member_order().size() < c.member_names.size()) {
            // Too few
            const auto& fnd = *decl.find_member(decl.member_order().back());
            const auto g = decl.ast_graph();
            out.push_back(sc::diag::Diagnostic::todo(g->text_info(), g->location(fnd.name_index)));
            return;
        } else if (decl.member_order().size() > c.member_names.size()) {
            // Too many
            const auto final_name = decl.member_order()[c.member_names.size()];
            const auto& fnd = *decl.find_member(final_name);
            const auto g = decl.ast_graph();
            out.push_back(sc::diag::Diagnostic::todo(g->text_info(), g->location(fnd.name_index)));
            return;
        }


        assert(c.member_names.size() == decl.member_order().size()); // checked above.

        size_t i { 0 };
        for (auto m : decl.member_order()) { }

        for (size_t i { 0 };; ++i) {
            if (i >= c.member_names.size())
                return;

            const auto& got_name = decl.member_order()[i];
            const auto& got = *decl.find_member(got_name);
            const auto& expected = c.member_names[i];

            if (got.expr_index) {
                //  Intrinsic classes shouldn't have defaults.
                const auto g = decl.ast_graph();
                out.push_back(sc::diag::Diagnostic::todo(g->text_info(), g->location(got.expr_index->second)));
                return;
            }
            if (got.name != expected) {
                //  Incorrect name
                const auto g = decl.ast_graph();
                const auto expected_name = cxt.symbol_table(expected);
                out.push_back(sc::diag::Diagnostic::intrinsic_class_wrong_member_name(
                    g->text_info(), g->location(got.name_index), expected_name));
                return;
            }
        }
    }();


    return out;
};


std::vector<sc::diag::Diagnostic> IRCompiler::compile_class_library( //
    const IntrinsicConstraints& intrinsic_constraints, //
    size_t num_threads) //
{
    (void)num_threads; // Later we should use a thread pool.

    if (m_compile_has_been_called) {
        // slowest memory order is fine here. This shouldn't happen
        assert(false);
        std::terminate();
    }
    m_compile_has_been_called = true;

    // Declare classes
    {
        size_t i { 0 };
        for (const auto& graph_and_index : ctx.class_library_ast) {
            const auto& graph = graph_and_index.first;

            const auto classes_or_extensions = graph->children(graph_and_index.second);

            for (auto child { classes_or_extensions.iter() }; child; ++child) {
                if (const auto class_i = graph->as_a<ast::ClassIndex>(*child)) {
                    auto result = ast2ir::DeclareClass { ctx, graph, ASTGraphIndex::from(i) }(*class_i);
                    if (result) {
                        auto& c = result->class_;
                        auto& m = result->meta_class_;

                        const auto valid_c = ctx.class_declarations.register_class_declaration(c);
                        const auto valid_m = ctx.class_declarations.register_class_declaration(m);
                        assert(valid_c == valid_m); // if not, we've added one without the other!
                        if (!valid_c) {
                            ctx.class_declarations.with_decl(c.name(), [&](const ClassDeclaration& other) {
                                ctx.diagnostics( //
                                    diag::Diagnostic::duplicate_class( //
                                        other.ast_graph()->text_info(), other.ast_graph()->location(other.name_index()),
                                        graph->text_info(),
                                        graph->location(c.name_index())) //
                                );
                            });
                        }
                        // Don't print for the meta class.
                    }
                }
            }
            i += 1;
        }
    }

    // add extentsion methods to decl
    {
        size_t i { 0 };
        for (const auto& graph_and_index : ctx.class_library_ast) {
            const auto& ast_graph_ptr = graph_and_index.first;
            const auto& ast_graph = *ast_graph_ptr;

            const auto classes_or_extensions = ast_graph.children(graph_and_index.second);

            for (auto child { classes_or_extensions.iter() }; child; ++child) {
                if (const auto ext_i = ast_graph.as_a<ast::ClassExtensionIndex>(*child)) {
                    const auto [class_name_i, method_list_i] = ast_graph.children(*ext_i);

                    // Inserts methods
                    const auto class_name = ctx.symbol_table.get_class_name(ast_graph.string(class_name_i));
                    ctx.class_declarations.with_decl(
                        class_name,
                        [&, method_list_i = method_list_i](ClassDeclaration& decl) {
                            for (auto method_it = ast_graph.children(method_list_i).iter(); method_it; ++method_it) {
                                if (auto meth_i = ast_graph.as_a<ast::MethodIndex>(*method_it)) {
                                    const auto [selector, args, maybe_prim, body] = ast_graph.children(*meth_i);
                                    const auto selector_sym = ctx.symbol_table.get_selector(ast_graph.string(selector));

                                    const auto r = decl.insert(ClassDeclaration::Method {
                                        ast_graph_ptr, //
                                        *meth_i, //
                                        selector_sym, //
                                        ast::Index::from(*selector) //
                                    });
                                    switch (r) {
                                    case ClassDeclaration::MethodInsertResult::Ok:
                                        break;
                                    case ClassDeclaration::MethodInsertResult::DuplicatedByMethod: {
                                        ctx.diagnostics(sc::diag::Diagnostic::todo(ast_graph_ptr->text_info(),
                                                                                   ast_graph.location(selector)));
                                        break;
                                    }
                                    case ClassDeclaration::MethodInsertResult::DuplicatedByExtentionMethod: {
                                        const auto* ptr = decl.find_method(selector_sym);
                                        assert(ptr); // shouldn't have told us this error if doesn't exist.
                                        ctx.diagnostics(sc::diag::Diagnostic::duplicate_extention(
                                            ast_graph.text_info(), ast_graph.location(selector),
                                            ptr->ast_graph->text_info(), ptr->ast_graph->location(ptr->name_index)));
                                        break;
                                    }
                                    case ClassDeclaration::MethodInsertResult::DuplicatedByAccessor: {
                                        ctx.diagnostics(sc::diag::Diagnostic::todo(ast_graph_ptr->text_info(),
                                                                                   ast_graph.location(selector)));
                                        break;
                                    }
                                    }
                                }
                            }
                        } //
                    );


                    // Inserts class methods
                    const auto meta_class_name = ctx.symbol_table.get_meta_class_name(ast_graph.string(class_name_i));
                    ctx.class_declarations.with_decl(
                        class_name,
                        [&, method_list_i = method_list_i](ClassDeclaration& decl) {
                            for (auto method_it = ast_graph.children(method_list_i).iter(); method_it; ++method_it) {
                                if (auto meth_i = ast_graph.as_a<ast::ClassMethodIndex>(*method_it)) {
                                    const auto [selector, args, maybe_prim, body] = ast_graph.children(*meth_i);
                                    const auto selector_sym = ctx.symbol_table.get_selector(ast_graph.string(selector));

                                    const auto r = decl.insert(ClassDeclaration::Method {
                                        ast_graph_ptr, //
                                        *meth_i, //
                                        selector_sym, //
                                        ast::Index::from(*selector) //
                                    });
                                    switch (r) {
                                    case ClassDeclaration::MethodInsertResult::Ok:
                                        break;
                                    case ClassDeclaration::MethodInsertResult::DuplicatedByMethod: {
                                        ctx.diagnostics(sc::diag::Diagnostic::todo(ast_graph_ptr->text_info(),
                                                                                   ast_graph.location(selector)));
                                        break;
                                    }
                                    case ClassDeclaration::MethodInsertResult::DuplicatedByExtentionMethod: {
                                        const auto* ptr = decl.find_method(selector_sym);
                                        assert(ptr); // shouldn't have told us this error if doesn't exist.
                                        ctx.diagnostics(sc::diag::Diagnostic::duplicate_extention(
                                            ast_graph.text_info(), ast_graph.location(selector),
                                            ptr->ast_graph->text_info(), ptr->ast_graph->location(ptr->name_index)));
                                        break;
                                    }
                                    case ClassDeclaration::MethodInsertResult::DuplicatedByAccessor: {
                                        ctx.diagnostics(sc::diag::Diagnostic::todo(ast_graph_ptr->text_info(),
                                                                                   ast_graph.location(selector)));
                                        break;
                                    }
                                    }
                                }
                            }
                        } //
                    );
                }
            }
            i += 1;
        }
    }


    // Check common classes exist

    bool common_missing = false;
    ctx.common_classes.for_each([&](SymSCClassName c) {
        if (auto decl = ctx.class_declarations.get_unsafe(c); !decl) {
            ctx.diagnostics(sc::diag::Diagnostic::missing_common_class(std::string { ctx.symbol_table(c) }));
            common_missing = true;
        }
    });


    // Check intrinsics, thread safe, but probably not needed.
    std::atomic<bool> intrinsic_constraints_met { true };
    for (const auto& [k, constraint] : intrinsic_constraints) {
        // We know this is okay to multithread because the constraints are a map, so we only ever access the decl
        // one at a time and we are no longer inserting any new ones.
        auto* decl = ctx.class_declarations.get_unsafe(k);
        if (!decl) {
            ctx.diagnostics(sc::diag::Diagnostic::missing_intrinsic_class(std::string { ctx.symbol_table(k) }));
            intrinsic_constraints_met = false;
            continue;
        }

        for (auto& diags : check_intrinsic_constraint(constraint, ctx, *decl)) {
            ctx.diagnostics(std::move(diags));
            intrinsic_constraints_met = false;
            decl->poison();
        }
    }


    // It is not safe to sort if these have not been satisfied.
    if (!intrinsic_constraints_met || common_missing) {
        ctx.class_declarations.poison_all();
        return ctx.diagnostics.consume();
    }


    // Step 2: Check hierarchy makes sense.

    // Chains don't intersect, this is thread safe.
    const auto cycles = ctx.inheritance_edges.cycle_check({ *ctx.common_classes.root_object });
    for (const auto& chain : cycles) {
        // TODO: create diagnostic for inheritance chain.
        std::optional<SymSCClassName> missing { };
        for (const auto c : chain) {
            if (auto decl = ctx.class_declarations.get_unsafe(c)) {
                decl->poison();
            } else {
                // Since it isn't possible to create an edge without defining a class and you can only have one
                // superclass, there should only ever be one missing superclass within a cycle
                assert(!missing);
                missing = c;
            }
        }

        if (missing) {
            if (!ctx.symbol_table.is_meta(*missing)) {
                // super class does not exist.
                std::vector<std::pair<std::shared_ptr<const sc::lex::TextInfo>, sc::lex::SourceCodeRange>> a;

                for (SymSCClassName child : ctx.inheritance_edges.get_unsafe(*missing)) {
                    if (auto child_ptr = ctx.class_declarations.get_unsafe(child)) {
                        const auto loc = child_ptr->super() ? child_ptr->super()->second : child_ptr->name_index();
                        const auto& child_graph = *child_ptr->ast_graph();
                        a.push_back({ child_graph.text_info(), child_graph.location(loc) });
                    }
                }

                ctx.diagnostics(sc::diag::Diagnostic::super_class_not_defined(
                    std::string { ctx.symbol_table(*missing) }, std::move(a)));
            }
        } else if (chain.size() == 1) {
            // Class inherits from itself.
            if (!ctx.symbol_table.is_meta(chain[0])) {
                auto ptr = ctx.class_declarations.get_unsafe(chain[0]);
                assert(ptr);
                assert(ptr->super());
                const auto super_loc = ptr->super()->second;
                const auto& graph = *ptr->ast_graph();
                ctx.diagnostics(sc::diag::Diagnostic::self_inheritance(graph.text_info(), graph.location(super_loc)));
            }
        } else {
            // Loop

            std::vector<std::pair<std::shared_ptr<const sc::lex::TextInfo>, sc::lex::SourceCodeRange>> a;

            for (const auto c : chain) {
                auto child_ptr = ctx.class_declarations.get_unsafe(c);
                assert(child_ptr);
                const auto loc = child_ptr->super() ? child_ptr->super()->second : child_ptr->name_index();
                const auto& child_graph = *child_ptr->ast_graph();
                a.push_back({ child_graph.text_info(), child_graph.location(loc) });
            }
            ctx.diagnostics(sc::diag::Diagnostic::inheritance_cycle(std::move(a)));
        }
    }


    // Step 3: Compile all ast class nodes.

    // There is exactly one thread that accesses each class declaration here.
    // We do not insert or remove any.
    // No need to lock when mutating (poisoning).
    for (auto class_it = ctx.class_declarations.unsafe_begin(); class_it != ctx.class_declarations.unsafe_end();
         ++class_it) {
        std::shared_ptr<IRGraph> class_ir { };
        const auto [name, decl] = *class_it;

        for (auto meth_it = decl.unsafe_methods_begin(); meth_it != decl.unsafe_methods_end(); ++meth_it) {
            const ast::ASTGraph& graph = *meth_it->second.ast_graph;
            const auto index = meth_it->second.ast_node_index;


            // TODO: compile method!


            //
        }
    }


    return ctx.diagnostics.consume();
}

void IRCompiler::insert_class_ir(SymSCClassName name, std::shared_ptr<IRGraph> graph) {
    std::scoped_lock lock { m_ir_graphs_lock };
    m_classes.insert({ name, std::move(graph) });
}


std::variant<ConstExpression, sc::diag::Diagnostic>
compile_consteval(std::shared_ptr<const ast::ASTGraph> ast_graph, ast::AnyExprIndex expr_i, SymbolTable& sym_table) {
    auto common_c = symbol_table_and_common_classes(sym_table, sym_table.get_class_name("AbstractObject"));
    std::vector<std::pair<std::shared_ptr<const ast::ASTGraph>, ast::ClassOrExtensionListIndex>> class_lib { };

    Context context { sym_table, std::move(common_c), class_lib };

    using R = std::variant<ConstExpression, sc::diag::Diagnostic>;
    auto r =
        std::visit(ast2ir::ConstEval { context, ast_graph, ASTGraphIndex { 0 } }, ast_graph->index_to_variant(expr_i));

    return std::move(r).visit([&](ast2ir::ConstEval::Success s) -> R { return { context.constant_table(s.first) }; },
                              [&](sc::diag::Diagnostic d) -> R { return { std::move(d) }; });
}

void InheritanceEdges::insert(SymSCClassName parent, SymSCClassName child) {
    std::scoped_lock l { m_lock };
    m_sub_classes[parent].push_back(child);
    // ensure child exists
    const auto& _ = m_sub_classes[child];

    // This means we
    assert(m_super_class.count(child) == 0);
    m_super_class.insert({ child, parent });
}

std::vector<std::vector<SymSCClassName>> InheritanceEdges::cycle_check(SymSCClassName root) const {
    std::scoped_lock l { m_lock };
    // Because root is empty, and we have single inheritance, it is impossible to create cycles that are attached to
    // root.
    std::unordered_set<SymSCClassName, std::hash<Symbol>> not_visited, visited;

    const auto dfs = [&](auto&& self, SymSCClassName n) -> void {
        assert(visited.count(n) == 0);
        not_visited.erase(n);
        visited.insert(n);

        for (const SymSCClassName child : m_sub_classes.at(n))
            self(self, child);
    };

    for (const auto& [k, v] : m_sub_classes) {
        not_visited.insert(k);
    }

    dfs(dfs, root);

    std::vector<std::vector<SymSCClassName>> loops;

    const auto loop_maker = [&](auto&& self, SymSCClassName n, std::vector<SymSCClassName>& current_cycle) -> void {
        // This is the end of a loop.
        assert(not_visited.count(n));
        current_cycle.push_back(n);
        not_visited.erase(n);

        for (const SymSCClassName child : m_sub_classes.at(n)) {
            if (not_visited.count(child) == 0)
                continue;
            self(self, child, current_cycle);
        }
    };

    // Effectively pulls a random class from the not_visited set and returns the cycle it is connected to.
    // Repeats until there are no more.
    while (!not_visited.empty()) {
        std::vector<SymSCClassName> cycles;
        SymSCClassName n = *not_visited.begin();
        std::unordered_set<SymSCClassName, std::hash<Symbol>> parent_visited;
        parent_visited.insert(n);

        while (true) {
            auto fnd = m_super_class.find(n);
            if (fnd == m_super_class.end())
                break;
            SymSCClassName s = fnd->second;
            if (parent_visited.count(s)) {
                break;
            }
            parent_visited.insert(s);
            n = s;
        }

        loop_maker(loop_maker, n, cycles);
        loops.push_back(std::move(cycles));
    }

    return loops;
}

const std::vector<SymSCClassName>& InheritanceEdges::get_unsafe(SymSCClassName n) const {
    assert(m_sub_classes.count(n));
    return m_sub_classes.at(n);
}
}
