//
// Created by antos07 on 11/9/23.
//

#include <cstdlib>
#include <format>
#include <fstream>
#include <iostream>
#include "Parser.hpp"
#include "Scanner.hpp"

int main (int argc, char *argv[]) {
    if (argc != 2) {
        std::cerr << std::format("Usage: {} <path to the source file>", argv[0]);
        return EXIT_FAILURE;
    }

    std::ifstream sourceFile{argv[1]};
    if (!sourceFile.is_open()) {
        // Failed to open the source file.
        std::perror(argv[0]);
        return EXIT_FAILURE;
    }

    calc::Scanner scanner{sourceFile, std::cerr};
    calc::Parser parser{&scanner};
    return parser.parse();
}
