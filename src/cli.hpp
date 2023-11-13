//
// Created by antos07 on 11/13/23.
//

#ifndef SYSPROG_LAB5_CLI_HPP
#define SYSPROG_LAB5_CLI_HPP

#include <string_view>

namespace cli {

bool has_option(int argc, const char * const *argv, std::string_view view);

bool is_debug_enabled(int argc, const char * const *argv);
}

#endif //SYSPROG_LAB5_CLI_HPP
