#pragma once
#include <cstdint>
#include <string_view>
#include <string>
#include <unordered_map>
#include <vector>

namespace sc::ir {

struct SymbolTable {
    using ID = std::uint32_t;
    std::vector<std::string_view> id_to_string_view;
    std::unordered_map<std::string, std::uint32_t> string_to_id;
};

}
