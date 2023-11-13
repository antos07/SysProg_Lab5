//
// Created by antos07 on 11/13/23.
//

#include "cli.hpp"

namespace cli {

bool has_option(const int argc, const char * const*argv, const std::string_view view) {
  for (int i = 0; i < argc; ++i) {
    if (view == argv[i]) {
      return true;
    }
  }
  return false;
}

bool is_debug_enabled(const int argc, const char * const*argv) {
  return has_option(argc, argv, "-d");
}
}
