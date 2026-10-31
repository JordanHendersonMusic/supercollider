
#pragma once

namespace sc::util {

/// Combines callables into one overload set, for `std::visit`.
/// @code
/// std::visit(sc::util::overload { [](int) {}, [](float) {} }, variant);
/// @endcode
template <typename... Ts> struct overload : Ts... { using Ts::operator()...; };
template <class... Ts> overload(Ts...) -> overload<Ts...>;

}
