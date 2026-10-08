#pragma once
#include "sc_diagnostic/sc_diagnostic.hpp"
#include "sc_parser/indexes_typed.hpp"
#include "sc_sema/ast_location.hpp"
#include "sc_sema/ir.hpp"
#include "sc_parser/ast.hpp"
#include "sc_sema/symbols.hpp"
#include "sc_sema/tables.hpp"
#include <mutex>
#include <atomic>
#include <thread>
#include <unordered_map>

namespace sc::ir {
using Diagnostic = sc::diag::Diagnostic;

struct CurrentDiagnostics {

    void operator()(Diagnostic diag) {
        std::scoped_lock lock{m_lock};
        m_diagnostics.push_back(std::move(diag));
    }

    [[nodiscard]] bool empty() const {
        std::scoped_lock lock{m_lock};
        return m_diagnostics.empty();
    }

    [[nodiscard]]  explicit operator bool() const {
        return empty();
    }

    [[nodiscard]] std::vector<Diagnostic> consume() {
        std::scoped_lock lock{m_lock};
        return std::move(m_diagnostics);
    }

private:
    mutable std::mutex m_lock { };
    std::vector<Diagnostic> m_diagnostics { };
};

// This is the runtime context.
// It is all the class library information, and the symbol and constant tables.
// When a irnode compiler pass needs to know something about the rest of the world, it goes through this.
struct Context {
    SymbolTable symbol_table { };
    ClassDeclarations class_declarations { };
    ConstantTable constant_table { };
    CurrentDiagnostics diagnostics { };
};

IRGraph create_graph(const ast::ASTGraph&, std::variant<ast::RegionListIndex, ast::ClassOrExtensionListIndex> roots,
                     ASTGraphIndex);

struct IntrinsicConstraints {
    SymSCClassName target;
    std::optional<SymSCClassName> super_class;
    std::optional<std::vector<SymSCNamedIdentifier>> member_names;
    std::optional<SymSCNamedIdentifier> slot_def;
};

struct IRCompiler {
    using GraphAndIndex = std::pair<ast::ASTGraph, ast::ClassOrExtensionListIndex>;

    IRCompiler( //
        std::unordered_map<SymSCClassName, IntrinsicConstraints, std::hash<Symbol>> intrinsic_constraints, //
        std::vector<GraphAndIndex> class_lib, //
        size_t num_threads = std::thread::hardware_concurrency());

private:
    Context context { };

    std::atomic<bool> m_compile_has_been_called { false };

    // Never mutated.
    const std::vector<std::pair<ast::ASTGraph, ast::ClassOrExtensionListIndex>> m_class_lib_ast;

    struct ClassData {
        SymSCClassName name;
        IRGraph ir_graph;
        SCClass_I class_index;
        bool poisoned { false };
    };

    void insert_class_ir(SymSCClassName name, IRGraph&& graph, SCClass_I class_index);

    void poison_class(SymSCClassName name);

    mutable std::mutex m_ir_graphs_lock { };
    std::unordered_map<SymSCClassName, ClassData, std::hash<Symbol>> m_classes { };
};


} // sc::ir
