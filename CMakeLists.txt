cmake_minimum_required(VERSION 3.26)
project(SysProg_Lab5)

set(CMAKE_CXX_STANDARD 20)

find_package(BISON)
find_package(FLEX)

BISON_TARGET(Parser src/parser.y ${CMAKE_CURRENT_BINARY_DIR}/Parser.cpp DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/Parser.hpp)
FLEX_TARGET(Scanner src/lexer.l ${CMAKE_CURRENT_BINARY_DIR}/Scanner.cpp DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/Scanner.hpp)
ADD_FLEX_BISON_DEPENDENCY(Scanner Parser)

add_executable(SysProg_Lab5
        ${BISON_Parser_OUTPUTS}
        ${FLEX_Scanner_OUTPUTS}
)

target_link_libraries(SysProg_Lab5 ${FLEX_LIBRARIES})
