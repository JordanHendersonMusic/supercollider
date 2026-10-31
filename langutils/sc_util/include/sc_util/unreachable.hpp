#pragma once

namespace sc::util {

/// @brief Marks a point that can never be reached, allowing the optimiser to assume it. Reaching it is undefined
/// behaviour. This is `std::unreachable`, from C++23.
[[noreturn]] inline void unreachable() {
#if defined(_MSC_VER) && !defined(__clang__)
    __assume(false);
#else
    __builtin_unreachable();
#endif
}

}
