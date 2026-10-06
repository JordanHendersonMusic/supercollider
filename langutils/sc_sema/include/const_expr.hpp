#pragma once
#include "symbol_table.hpp"
#include <cstdint>
#include <limits>
#include <utility>
#include <variant>

#include "strong_index.hpp"

namespace sc::ir {


using ConstString = sc::util::StrongIndex<SymbolTable::ID, struct ConstString__>;
using ConstSymbol = sc::util::StrongIndex<SymbolTable::ID, struct ConstSymbol__>;
using ConstClassName = sc::util::StrongIndex<SymbolTable::ID, struct ConstClassName__>;

struct ConstExpression;

using ConstExprArray = std::vector<ConstExpression>;
using ConstExprEvent = std::vector<std::pair<ConstExpression, ConstExpression>>;

using ConstStorage = std::variant<double, std::int32_t, char, bool, ConstSymbol, ConstString, ConstClassName,
                                  ConstExprArray, ConstExprEvent>;


using ConstExprIndex = sc::util::StrongIndex<std::uint32_t, struct ConstantExpressionIndex__>;
using OptionalConstExprIndex = sc::util::StrongOptionalIndex<std::uint32_t, struct ConstantExpressionIndex__,
                                                             std::numeric_limits<std::uint32_t>::max()>;

struct ConstExpression {
    constexpr ConstExpression(double d): storage(d) {}
    constexpr ConstExpression(std::int32_t d): storage(d) {}
    constexpr ConstExpression(char d): storage(d) {}
    constexpr ConstExpression(bool d): storage(d) {}
    constexpr ConstExpression(ConstSymbol d): storage(d) {}
    constexpr ConstExpression(ConstString d): storage(d) {}
    constexpr ConstExpression(ConstClassName d): storage(d) {}
    ConstExpression(ConstExprArray d) noexcept: storage(std::move(d)) {}
    ConstExpression(ConstExprEvent d) noexcept: storage(std::move(d)) {}

    ConstExpression(ConstExpression&&) noexcept = default;
    ConstExpression(const ConstExpression&) = delete;
    ConstExpression& operator=(ConstExpression&&) = delete;
    ConstExpression& operator=(const ConstExpression&) = delete;

    ~ConstExpression() = default;

    template <typename F> decltype(auto) visit(F&& f) const { return std::visit(std::forward<F>(f), storage); }

private:
    ConstStorage storage;
};

}
