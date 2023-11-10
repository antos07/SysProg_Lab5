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

%token COMMA                        ","
%token COLON                        ":"
%token SEMICOLON                    ";"
%token LBRACE                       "{"
%token RBRACE                       "}"
%token LPAREN                       "("
%token RPAREN                       ")"
%token LSBRACKET                    "["
%token RSBRACKET                    "]"
%token QUESTION                     "?"
%token POINT                        "."
%token ARROW                        "->"
%token ELLIPSIS                     "..."

%token PLUS                         "+"
%token MINUS                        "-"
%token ASTERISK                     "*"
%token SLASH                        "/"
%token PERCENT                      "%"

%token INCREMENT                    "++"
%token DECREMENT                    "--"

%token LNOT                         "!"
%token LOR                          "||"
%token LAND                         "&&"
%token EQUAL                        "=="
%token NEQUAL                       "!="
%token LESS                         "<"
%token GREATER                      ">"
%token LEQUAL                       "<="
%token GEQUAL                       ">="

%token BOR                          "|"
%token BAND                         "&"
%token BXOR                         "^"
%token BINV                         "~"
%token LSHIFT                       "<<"
%token RSHIFT                       ">>"

%token ASSIGNMENT                   "="
%token MUL_ASSIGNMENT               "*="
%token DIV_ASSIGNMENT               "/="
%token MOD_ASSIGNMENT               "%="
%token PLUS_ASSIGNMENT              "+="
%token MINUS_ASSIGNMENT             "-="
%token LSHIFT_ASSIGNMENT            "<<="
%token RSHIFT_ASSIGNMENT            ">>="
%token BAND_ASSIGNMENT              "&="
%token BOR_ASSIGNMENT               "|="
%token BXOR_ASSIGNMENT              "^="

%token <std::string> IDENTIFIER
%token <std::string> STRING
%token <std::string> INTEGER_CONSTANT
%token <std::string> CHARACTER_CONSTANT
%token <std::string> FLOATING_CONSTANT

%token AUTO                         "auto"
%token REGISTER                     "register"
%token STATIC                       "static"
%token EXTERN                       "extern"
%token TYPEDEF                      "typedef"

%token STRUCT                       "struct"
%token UNION                        "union"
%token ENUM                         "enum"
%token CASE                         "case"
%token DEFAULT                      "default"
%token IF                           "if"
%token ELSE                         "else"
%token SWITCH                       "switch"
%token WHILE                        "while"
%token DO                           "do"
%token FOR                          "for"
%token GOTO                         "goto"
%token CONTINUE                     "continue"
%token BREAK                        "break"
%token RETURN                       "return"

%token CONST                        "const"
%token VOLATILE                     "volatile"

%token VOID                         "void"
%token CHAR                         "char"
%token SHORT                        "short"
%token INT                          "int"
%token LONG                         "long"
%token FLOAT                        "float"
%token DOUBLE                       "double"
%token SIGNED                       "signed"
%token UNSIGNED                     "unsigned"

%token SIZEOF                       "sizeof"

%%

translation-unit            : translation-unit external-declaration
                            | %empty
                            ;

external-declaration        : function-definition
                            | declaration
                            ;

function-definition         : declaration-specifiers declarator declarations compound-statement
                            | declaration-specifiers declarator compound-statement
                            | declarator declarations compound-statement
                            | declarator compound-statement
                            ;

declaration-specifiers      : declaration-specifier
                            | declaration-specifiers declaration-specifier
                            ;

declarations                : declarations declaration
                            | declaration
                            ;

declaration-specifier       : storage-class-specifier
                            | type-specifier
                            | type-qualifier
                            ;

storage-class-specifier     : AUTO
                            | REGISTER
                            | STATIC
                            | EXTERN
                            | TYPEDEF
                            ;

type-specifier              : VOID
                            | CHAR
                            | SHORT
                            | INT
                            | LONG
                            | FLOAT
                            | DOUBLE
                            | SIGNED
                            | UNSIGNED
                            | struct-or-union-specifier
                            | enum-specifier
                            | typedef-name
                            ;

struct-or-union-specifier   : struct-or-union IDENTIFIER LBRACE struct-declarations RBRACE
                            | struct-or-union LBRACE struct-declarations RBRACE
                            | struct-or-union IDENTIFIER
                            ;

struct-declarations         : struct-declarations struct-declaration
                            | struct-declaration
                            ;

struct-or-union             : STRUCT
                            | UNION
                            ;

struct-declaration          : specifier-qualifiers struct-declarator-list
                            | struct-declarator-list
                            ;

specifier-qualifiers        : specifier-qualifiers specifier-qualifier
                            | specifier-qualifier
                            ;

specifier-qualifier         : type-specifier
                            | type-qualifier
                            ;

struct-declarator-list      : struct-declarator
                            | struct-declarator-list COMMA struct-declarator
                            ;

struct-declarator           : declarator
                            | declarator COLON constant-expression
                            | COLON constant-expression
                            ;

declarator                  : direct-declarator
                            | pointer direct-declarator
                            ;

pointer                     : ASTERISK
                            | ASTERISK type-qualifiers
                            | ASTERISK pointer
                            | ASTERISK type-qualifiers pointer
                            ;

type-qualifiers             : type-qualifiers type-qualifier
                            | type-qualifier
                            ;

type-qualifier              : CONST
                            | VOLATILE
                            ;

direct-declarator           : IDENTIFIER
                            | LPAREN declarator RPAREN
                            | direct-declarator LSBRACKET RSBRACKET
                            | direct-declarator LSBRACKET constant-expression RSBRACKET
                            | direct-declarator LPAREN parameter-type-list RPAREN
                            | direct-declarator LPAREN identifiers RPAREN
                            ;

identifiers                 : identifiers IDENTIFIER
                            | %empty
                            ;

constant-expression         : conditional-expression
                            ;

conditional-expression      : logical-or-expression
                            | logical-or-expression QUESTION expression COLON conditional-expression
                            ;

logical-or-expression       : logical-and-expression
                            | logical-or-expression LOR logical-and-expression
                            ;

logical-and-expression      : inclusive-or-expression
                            | logical-and-expression LAND inclusive-or-expression
                            ;

inclusive-or-expression     : exclusive-or-expression
                            | inclusive-or-expression BOR exclusive-or-expression
                            ;

exclusive-or-expression     : and-expression
                            | exclusive-or-expression BXOR and-expression
                            ;

and-expression              : equality-expression
                            | and-expression BAND equality-expression
                            ;

equality-expression         : relational-expression
                            | equality-expression EQUAL relational-expression
                            | equality-expression NEQUAL relational-expression
                            ;

relational-expression       : shift-expression
                            | relational-expression LESS shift-expression
                            | relational-expression GREATER shift-expression
                            | relational-expression LEQUAL shift-expression
                            | relational-expression GEQUAL shift-expression
                            ;

shift-expression            : additive-expression
                            | shift-expression LSHIFT additive-expression
                            | shift-expression RSHIFT additive-expression
                            ;

additive-expression         : multiplicative-expression
                            | additive-expression PLUS multiplicative-expression
                            | additive-expression MINUS multiplicative-expression
                            ;

multiplicative-expression   : cast-expression
                            | multiplicative-expression ASTERISK cast-expression
                            | multiplicative-expression SLASH cast-expression
                            | multiplicative-expression PERCENT cast-expression
                            ;

cast-expression             : unary-expression
                            | LPAREN type-name RPAREN cast-expression
                            ;

unary-expression            : postfix-expression
                            | INCREMENT unary-expression
                            | DECREMENT unary-expression
                            | unary-operator cast-expression
                            | SIZEOF unary-expression
                            | SIZEOF type-name
                            ;

postfix-expression          : primary-expression
                            | postfix-expression LSBRACKET expression RSBRACKET
                            | postfix-expression LPAREN assignment-expressions RPAREN
                            | postfix-expression POINT IDENTIFIER
                            | postfix-expression ARROW IDENTIFIER
                            | postfix-expression INCREMENT
                            | postfix-expression DECREMENT
                            ;

assignment-expressions      : assignment-expressions assignment-expression
                            | %empty
                            ;

primary-expression          : IDENTIFIER
                            | constant
                            | STRING
                            | LPAREN expression RPAREN
                            ;

constant                    : INTEGER_CONSTANT
                            | CHARACTER_CONSTANT
                            | FLOATING_CONSTANT
                            ;

expression                  : assignment-expression
                            | expression COMMA assignment-expression
                            ;

assignment-expression       : conditional-expression
                            | unary-expression assignment-operator assignment-expression
                            ;

assignment-operator         : ASSIGNMENT
                            | MUL_ASSIGNMENT
                            | DIV_ASSIGNMENT
                            | MOD_ASSIGNMENT
                            | PLUS_ASSIGNMENT
                            | MINUS_ASSIGNMENT
                            | LSHIFT_ASSIGNMENT
                            | RSHIFT_ASSIGNMENT
                            | BAND_ASSIGNMENT
                            | BOR_ASSIGNMENT
                            | BXOR_ASSIGNMENT
                            ;

unary-operator              : BAND
                            | ASTERISK
                            | PLUS
                            | MINUS
                            | BINV
                            | LNOT
                            ;

type-name                   : specifier-qualifiers abstract-declarator
                            | specifier-qualifiers
                            ;


parameter-type-list         : parameter-list
                            | parameter-list COMMA ELLIPSIS
                            ;

parameter-list              : parameter-declaration
                            | parameter-list COMMA parameter-declaration
                            ;

parameter-declaration       : declaration-specifiers declarator
                            | declaration-specifiers abstract-declarator
                            | declaration-specifiers
                            ;

abstract-declarator         : pointer
                            | pointer direct-abstract-declarator
                            | direct-abstract-declarator
                            ;

direct-abstract-declarator  : LPAREN abstract-declarator RPAREN
                            | LSBRACKET RSBRACKET
                            | direct-abstract-declarator LSBRACKET RSBRACKET
                            | LSBRACKET constant-expression RSBRACKET
                            | direct-abstract-declarator LSBRACKET constant-expression RSBRACKET
                            | LPAREN RPAREN
                            | direct-abstract-declarator LPAREN RPAREN
                            | LPAREN parameter-type-list RPAREN
                            | direct-abstract-declarator LPAREN parameter-type-list RPAREN
                            ;

enum-specifier              : ENUM IDENTIFIER LBRACE enumerator-list RBRACE
                            | ENUM LBRACE enumerator-list RBRACE
                            | ENUM IDENTIFIER
                            ;

enumerator-list             : enumerator
                            | enumerator-list COMMA enumerator
                            ;

enumerator                  : IDENTIFIER
                            | IDENTIFIER ASSIGNMENT constant-expression
                            ;

typedef-name                : IDENTIFIER
                            ;

declaration                 : declaration-specifier-req init-declarators
                            ;

declaration-specifier-req   : declaration-specifiers declaration-specifier
                            ;

init-declarators            : init-declarators init-declarator
                            | %empty
                            ;

init-declarator             : declarator
                            | declarator ASSIGNMENT initializer
                            ;

initializer                 : assignment-expression
                            | LBRACE initializer-list RBRACE
                            | LBRACE initializer-list COMMA RBRACE
                            ;

initializer-list            : initializer
                            | initializer-list COMMA initializer
                            ;

compound-statement          : LBRACE declarations statements RBRACE
                            ;

statements                  : statements statement
                            | %empty
                            ;

statement                   : labeled-statement
                            | expression-statement
                            | compound-statement
                            | selection-statement
                            | iteration-statement
                            | jump-statement
                            ;

labeled-statement           : IDENTIFIER COLON statement
                            | CASE constant-expression COLON statement
                            | DEFAULT COLON statement
                            ;

expression-statement        : expression
                            | %empty
                            ;

selection-statement         : IF LPAREN expression RPAREN statement
                            | IF LPAREN expression RPAREN statement ELSE statement
                            | SWITCH LPAREN expression RPAREN statement
                            ;

iteration-statement         : WHILE LPAREN expression RPAREN statement
                            | DO statement WHILE LPAREN expression RPAREN SEMICOLON
                            | FOR LPAREN expression SEMICOLON expression SEMICOLON expression RPAREN statement
                            | FOR LPAREN SEMICOLON expression SEMICOLON expression RPAREN statement
                            | FOR LPAREN expression SEMICOLON SEMICOLON expression RPAREN statement
                            | FOR LPAREN SEMICOLON SEMICOLON expression RPAREN statement
                            | FOR LPAREN expression SEMICOLON expression SEMICOLON RPAREN statement
                            | FOR LPAREN SEMICOLON expression SEMICOLON RPAREN statement
                            | FOR LPAREN expression SEMICOLON SEMICOLON RPAREN statement
                            | FOR LPAREN SEMICOLON SEMICOLON RPAREN statement
                            ;

jump-statement              : GOTO IDENTIFIER SEMICOLON
                            | CONTINUE SEMICOLON
                            | BREAK SEMICOLON
                            | RETURN expression SEMICOLON
                            | RETURN SEMICOLON
                            ;

%%

void calc::Parser::error(const std::string& msg) {
    std::cerr << "Line " << scanner->lineno() << ": " << msg << '\n';
}
