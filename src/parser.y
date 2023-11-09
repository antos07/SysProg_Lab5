%{
#include <iostream>
#include <string>
#include <cmath>
#include <string>
#include <FlexLexer.h>
%}

%require "3.8.2"
%language "C++"
%defines "Parser.hpp"
%output "Parser.cpp"

%define api.parser.class {Parser}
%define api.namespace {calc}
%define api.value.type variant
%define parse.error detailed
%parse-param {Scanner* scanner}

%code requires
{
    namespace calc {
        class Scanner;
    } // namespace calc
} // %code requires

%code
{
    #include "Scanner.hpp"
    #define yylex(x) scanner->lex(x)
}

%token SEMICOLON ";"

%token <std::string> IDENTIFIER

%token VOID
%token CHAR
%token SHORT
%token INT
%token LONG
%token FLOAT
%token DOUBLE
%token SIGNED
%token UNSIGNED

%%

translation-unit        : %empty
                        | translation-unit external-declaration
                        ;

external-declaration    : declaration
                        ;

declaration             : declaration-specifiers init-declarations SEMICOLON;

declaration-specifiers  : declaration-specifiers declaration-specifier
                        | declaration-specifier
                        ;

declaration-specifier   : type-specifier
                        ;


type-specifier          : VOID
                        | CHAR
                        | SHORT
                        | INT
                        | LONG
                        | FLOAT
                        | DOUBLE
                        | SIGNED
                        | UNSIGNED
                        ;


init-declarations       : IDENTIFIER
                        ;

%%

void calc::Parser::error(const std::string& msg) {
    std::cerr << "Line " << scanner->lineno() << ": " << msg << '\n';
}
