#pragma once
#include "SCBase.h"
#include "SC_Version.hpp"

namespace v_errors {

struct PassingNilToLiteralInitArg {
    static constexpr auto version { SemanticVersion { 3, 16, 0 } };
    constexpr static bool do_new() { return SC_Version >= PassingNilToLiteralInitArg::version; }
    static void print_error() {
        post("ERROR: in version 3.16 and onwards, passing nil to a default initialised argument will have "
             "different behaviour.\n"
             "See **** for more information and for how to update your code.\n");
    }
};

}
