#pragma once

#include "sc_diagnostic/sc_diagnostic.hpp"
#include "sc_parser/ast.hpp"
#include "sc_parser/indexes_typed.hpp"
#include "sc_sema/const_expr.hpp"
#include "sc_sema/decls.hpp"
#include "sc_sema/symbols.hpp"
#include <mutex>

namespace sc::ast {
struct ASTGraphAndValidIndex;
}

namespace sc::ir {
struct CurrentDiagnostics {
    void operator()(sc::diag::Diagnostic diag) {
        std::scoped_lock lock { m_lock };
        m_diagnostics.push_back(std::move(diag));
    }

    void operator()(std::vector<sc::diag::Diagnostic> diags) {
        std::scoped_lock lock { m_lock };
        for (auto& diag : diags)
            m_diagnostics.push_back(std::move(diag));
    }

    [[nodiscard]] bool empty() const {
        std::scoped_lock lock { m_lock };
        return m_diagnostics.empty();
    }

    [[nodiscard]] explicit operator bool() const { return empty(); }

    [[nodiscard]] std::vector<sc::diag::Diagnostic> consume() {
        std::scoped_lock lock { m_lock };
        return std::move(m_diagnostics);
    }

private:
    mutable std::mutex m_lock { };
    std::vector<sc::diag::Diagnostic> m_diagnostics { };
};


struct CommonClasses {
    SymSCClassName root_object;
    SymSCClassName default_inheritor;
    SymSCClassName class_;
    SymSCClassName string;
    SymSCClassName true_;
    SymSCClassName false_;
    SymSCClassName nil;
    SymSCClassName float_;
    SymSCClassName int_;
    SymSCClassName char_;
    SymSCClassName symbol;
    SymSCClassName array;
    SymSCClassName event;

    template <typename F> void for_each(F f) const {
        // We could cast this into a SymSCClassName*... ?
        f(root_object);
        f(default_inheritor);
        f(class_);
        f(string);
        f(true_);
        f(false_);
        f(nil);
        f(float_);
        f(int_);
        f(char_);
        f(symbol);
    }
};

struct InheritanceEdges {
    void insert(SymSCClassName parent, SymSCClassName child);
    void insert(SymSCClassName none) { m_sub_classes[none]; };

    /// Requires root has no parents.
    /// Returns inheritance cycles.
    /// If empty, no cycles.
    std::vector<std::vector<SymSCClassName>> cycle_check(SymSCClassName root) const;

    // Not thread safe.
    const std::vector<SymSCClassName>& get_unsafe(SymSCClassName n) const;

private:
    mutable std::mutex m_lock { };
    std::unordered_map<SymSCClassName, std::vector<SymSCClassName>, std::hash<Symbol>> m_sub_classes { };
    std::unordered_map<SymSCClassName, SymSCClassName, std::hash<Symbol>> m_super_class { };
};


struct SymbolTable;

struct Context {
    Context(SymbolTable& sym_table, CommonClasses com,
            std::vector<std::pair<std::shared_ptr<const ast::ASTGraph>, ast::ClassOrExtensionListIndex>>& class_library):
        symbol_table(sym_table),
        common_classes(com),
        class_library_ast(class_library) { }

    SymbolTable& symbol_table;
    CommonClasses common_classes;
    ClassDeclarations class_declarations { };
    std::vector<std::pair<std::shared_ptr<const ast::ASTGraph>, ast::ClassOrExtensionListIndex>>& class_library_ast;
    ConstantTable constant_table { };
    CurrentDiagnostics diagnostics { };
    InheritanceEdges inheritance_edges { };
};

}