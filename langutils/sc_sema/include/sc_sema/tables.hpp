#pragma once
#include "sc_sema/const_expr.hpp"
#include "sc_sema/symbols.hpp"
#include "sc_sema/type_info.hpp"
#include <mutex>
#include <string>
#include <string_view>

namespace sc::ir {

////////////////////////////////////////////////////////////////////////////////
// symbol table
////////////////////////////////////////////////////////////////////////////////

struct SymbolTable {
    // TODO: use boost unordered flat map (when we can figure out how the includes work in sc...)

    template <SymbolTypes T> SymbolDef::TypedIndex<T> get(std::string s) {
        return this->template operator()<T>(std::move(s));
    }

    template <typename T> T get(std::string s) { 
        static_assert(T::Possible.size() == 1);
        return this->template operator()<T::Possible[0]>(std::move(s)); 
    }

    template <SymbolTypes T> SymbolDef::TypedIndex<T> operator()(std::string s) {
        std::scoped_lock lock { m_lock };
        if (const auto fnd = m_string_to_id.find(s); fnd != m_string_to_id.end()) {
            return { *fnd->second };
        }
        const auto new_id = SymbolDef::TypedIndex<T>::from(m_id_to_string_view.size());
        const std::string_view view { s };
        m_string_to_id.insert({ std::move(s), static_cast<Symbol>(new_id) });
        m_id_to_string_view.push_back(view);
        return new_id;
    }

    [[nodiscard]] std::string_view operator()(Symbol i) const noexcept {
        std::scoped_lock lock { m_lock };
        return m_id_to_string_view[*i];
    }

private:
    mutable std::mutex m_lock { };
    std::vector<std::string_view> m_id_to_string_view { };
    std::unordered_map<std::string, Symbol> m_string_to_id { };
};

////////////////////////////////////////////////////////////////////////////////
// Class declarations
////////////////////////////////////////////////////////////////////////////////

struct ClassDeclaration {
    struct Member {
        SymSCSelector name;
        bool externally_readable;
        bool externally_writeable;
        bool internally_writeable; // aka, a const
        std::optional<ConstExprIndex> expr_index;
        std::optional<TypeInfo> type_info;
    };
    SymSCClassName name;
    std::optional<SymSCClassName> super { };
    // Meta classes don't have meta classes.
    std::optional<SymSCClassName> meta_class {};
    std::vector<Member> members { };
    std::vector<SymSCSelector> consteval_methods { };
    bool poisoned { false };
};


struct ClassDeclarations {
    void register_class_declaration(ClassDeclaration decl) {
        std::scoped_lock lock { m_lock };
        const auto name = decl.name;
        m_class_declarations.insert({ name, std::move(decl) });
    }

    [[nodiscard]] bool exists(SymSCClassName name) const {
        return m_class_declarations.find(name) != m_class_declarations.end();
    }

    /// This will lock.
    /// Requires name be registered
    template <typename F> auto with_decl(SymSCClassName name, F&& f) {
        std::scoped_lock lock { m_lock };
        return std::invoke(std::forward<F>(f), m_class_declarations.find(name)->second);
    }

    /// This will lock.
    /// Requires name be registered
    template <typename F> auto with_decl(SymSCClassName name, F&& f) const {
        std::scoped_lock lock { m_lock };
        return std::invoke(std::forward<F>(f), m_class_declarations.find(name)->second);
    }

private:
    mutable std::mutex m_lock;
    std::unordered_map<SymSCClassName, ClassDeclaration, std::hash<Symbol>> m_class_declarations { };
};

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

struct ConstantTable {
    [[nodiscard]] ConstExprIndex register_constexpr(ConstExpression&& expr) {
        std::scoped_lock lock { m_lock };
        if (const auto fnd = expr_to_index.find(expr); fnd != expr_to_index.end()) {
            return fnd->second;
        }
        const auto index = m_rolling;
        m_rolling += 1;
        expr_to_index.insert({ expr, index });
        index_to_expr.insert({ index, expr });
        return index;
    }

    // this is marked noexcept because you should never construct an index yourself.
    [[nodiscard]] const ConstExpression& operator()(ConstExprIndex index) const noexcept {
        std::scoped_lock lock { m_lock };
        return index_to_expr.at(index);
    }

private:
    mutable std::mutex m_lock;
    ConstExprIndex::Underlying m_rolling { 0 };
    // This means we duplicate the constexpr.
    // This could probably be optimized.
    std::unordered_map<ConstExprIndex, ConstExpression> index_to_expr { };
    std::unordered_map<ConstExpression, ConstExprIndex> expr_to_index { };
};

}
