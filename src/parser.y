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
%define api.namespace {parsing}
%define api.value.type variant
%define parse.error detailed
%parse-param {Scanner* scanner}

%code requires
{
    namespace parsing {
        class Scanner;
    } // namespace parsing
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
%token RESTRICT                     "restrict"

%token INLINE                       "inline"

%token VOID                         "void"
%token CHAR                         "char"
%token SHORT                        "short"
%token INT                          "int"
%token LONG                         "long"
%token FLOAT                        "float"
%token DOUBLE                       "double"
%token SIGNED                       "signed"
%token UNSIGNED                     "unsigned"
%token BOOL                         "bool"

%token SIZEOF                       "sizeof"

%%

translation_unit
	: %empty
	| translation_unit external_declaration
	;

external_declaration
	: function_definition
	| declaration
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement
	| declaration_specifiers declarator compound_statement
	;

declaration
	: declaration_specifiers SEMICOLON
	| declaration_specifiers init_declarator_list SEMICOLON
	;

declaration_specifiers
	: storage_class_specifier declaration_specifiers
	| storage_class_specifier
	| type_specifier declaration_specifiers
	| type_specifier
	| type_qualifier declaration_specifiers
	| type_qualifier
	| function_specifier declaration_specifiers
	| function_specifier
	;

declarator
	: pointer direct_declarator
	| direct_declarator
	;

declaration_list
	: declaration
	| declaration_list declaration
	;

compound_statement
	: LBRACE RBRACE
	| LBRACE block_item_list RBRACE
	;

init_declarator_list
	: init_declarator
	| init_declarator_list COMMA init_declarator
	;

storage_class_specifier
	: TYPEDEF
	| EXTERN
	| STATIC
	| AUTO
	| REGISTER
	;

type_specifier
	: VOID
	| CHAR
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| SIGNED
	| UNSIGNED
	| BOOL
	| struct_or_union_specifier
	| enum_specifier
	/* | IDENTIFIER	—  removed support for typedefs*/
	;

type_qualifier
	: CONST
	| RESTRICT
	| VOLATILE
	;

function_specifier
	: INLINE
	;

pointer
	: ASTERISK type_qualifier_list pointer
	| ASTERISK type_qualifier_list
	| ASTERISK pointer
	| ASTERISK
	;

direct_declarator
	: IDENTIFIER
	| LPAREN declarator RPAREN
	| direct_declarator LSBRACKET RSBRACKET
	| direct_declarator LSBRACKET ASTERISK RSBRACKET
	| direct_declarator LSBRACKET STATIC type_qualifier_list assignment_expression RSBRACKET
	| direct_declarator LSBRACKET STATIC assignment_expression RSBRACKET
	| direct_declarator LSBRACKET type_qualifier_list ASTERISK RSBRACKET
	| direct_declarator LSBRACKET type_qualifier_list STATIC assignment_expression RSBRACKET
	| direct_declarator LSBRACKET type_qualifier_list assignment_expression RSBRACKET
	| direct_declarator LSBRACKET type_qualifier_list RSBRACKET
	| direct_declarator LSBRACKET assignment_expression RSBRACKET
	| direct_declarator LPAREN parameter_type_list RPAREN
	| direct_declarator LPAREN RPAREN
	| direct_declarator LPAREN identifier_list RPAREN
	;

block_item_list
	: block_item
	| block_item_list block_item
	;

init_declarator
	: declarator ASSIGNMENT initializer
	| declarator
	;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression
	;

parameter_type_list
	: parameter_list COMMA ELLIPSIS
	| parameter_list
	;

identifier_list
	: IDENTIFIER
	| identifier_list COMMA IDENTIFIER
	;

block_item
	: declaration
	| statement
	;

initializer
	: LBRACE initializer_list RBRACE
	| LBRACE initializer_list COMMA RBRACE
	| assignment_expression
	;

conditional_expression
	: logical_or_expression
	| logical_or_expression QUESTION expression COLON conditional_expression
	;

unary_expression
	: postfix_expression
	| INCREMENT unary_expression
	| DECREMENT unary_expression
	| unary_operator cast_expression
	| SIZEOF unary_expression
	| SIZEOF LPAREN type_name RPAREN
	;

assignment_operator
	: ASSIGNMENT
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

parameter_list
	: parameter_declaration
	| parameter_list COMMA parameter_declaration
	;

statement
	: labeled_statement
	| compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	;

initializer_list
	: designation initializer
	| initializer
	| initializer_list COMMA designation initializer
	| initializer_list COMMA initializer
	;

logical_or_expression
	: logical_and_expression
	| logical_or_expression LOR logical_and_expression
	;

postfix_expression
	: primary_expression
	| postfix_expression LSBRACKET expression RSBRACKET
	| postfix_expression LPAREN RPAREN
	| postfix_expression LPAREN argument_expression_list RPAREN
	| postfix_expression POINT IDENTIFIER
	| postfix_expression ARROW IDENTIFIER
	| postfix_expression INCREMENT
	| postfix_expression DECREMENT
	| LPAREN type_name RPAREN RBRACE initializer_list RBRACE
	| LPAREN type_name RPAREN RBRACE initializer_list COMMA RBRACE
	;

unary_operator
	: BAND
	| ASTERISK
	| PLUS
	| MINUS
	| BINV
	| LNOT
	;

cast_expression
	: unary_expression
	| LPAREN type_name RPAREN cast_expression
	;

type_name
	: specifier_qualifier_list abstract_declarator
	| specifier_qualifier_list
	;

parameter_declaration
	: declaration_specifiers declarator
	| declaration_specifiers abstract_declarator
	| declaration_specifiers
	;

labeled_statement
	: IDENTIFIER COLON statement
	| CASE constant_expression COLON statement
	| DEFAULT COLON statement
	;

expression_statement
	: SEMICOLON
	| expression SEMICOLON
	;

selection_statement
	: IF LPAREN expression RPAREN statement ELSE statement
	| IF LPAREN expression RPAREN statement
	| SWITCH LPAREN expression RPAREN statement
	;

iteration_statement
	: WHILE LPAREN expression RPAREN statement
	| DO statement WHILE LPAREN expression RPAREN SEMICOLON
	| FOR LPAREN expression_statement expression_statement RPAREN statement
	| FOR LPAREN expression_statement expression_statement expression RPAREN statement
	| FOR LPAREN declaration expression_statement RPAREN statement
	| FOR LPAREN declaration expression_statement expression RPAREN statement
	;

jump_statement
	: GOTO IDENTIFIER SEMICOLON
	| CONTINUE SEMICOLON
	| BREAK SEMICOLON
	| RETURN SEMICOLON
	| RETURN expression SEMICOLON
	;

designation
	: designator_list ASSIGNMENT
	;

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression LAND inclusive_or_expression
	;

primary_expression
	: IDENTIFIER
	| constant
	| STRING
	| LPAREN expression RPAREN
	;

expression
	: assignment_expression
	| expression COMMA assignment_expression
	;

argument_expression_list
	: assignment_expression
	| argument_expression_list COMMA assignment_expression
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list
	| type_specifier
	| type_qualifier specifier_qualifier_list
	| type_qualifier
	;

abstract_declarator
	: pointer direct_abstract_declarator
	| pointer
	| direct_abstract_declarator
	;

constant_expression
	: conditional_expression
	;

designator_list
	: designator
	| designator_list designator
	;

inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression BOR exclusive_or_expression
	;

constant
	: INTEGER_CONSTANT		/* includes character_constant */
	| FLOATING_CONSTANT
	/* | IDENTIFIER	    remove support of enumeration constants */
	;

direct_abstract_declarator
	: LPAREN abstract_declarator RPAREN
	| LSBRACKET RSBRACKET
	| LSBRACKET ASTERISK RSBRACKET
	| LSBRACKET STATIC type_qualifier_list assignment_expression RSBRACKET
	| LSBRACKET STATIC assignment_expression RSBRACKET
	| LSBRACKET type_qualifier_list STATIC assignment_expression RSBRACKET
	| LSBRACKET type_qualifier_list assignment_expression RSBRACKET
	| LSBRACKET type_qualifier_list RSBRACKET
	| LSBRACKET assignment_expression RSBRACKET
	| direct_abstract_declarator LSBRACKET RSBRACKET
	| direct_abstract_declarator LSBRACKET ASTERISK RSBRACKET
	| direct_abstract_declarator LSBRACKET STATIC type_qualifier_list assignment_expression RSBRACKET
	| direct_abstract_declarator LSBRACKET STATIC assignment_expression RSBRACKET
	| direct_abstract_declarator LSBRACKET type_qualifier_list assignment_expression RSBRACKET
	| direct_abstract_declarator LSBRACKET type_qualifier_list STATIC assignment_expression RSBRACKET
	| direct_abstract_declarator LSBRACKET type_qualifier_list RSBRACKET
	| direct_abstract_declarator LSBRACKET assignment_expression RSBRACKET
	| LPAREN RPAREN
	| LPAREN parameter_type_list RPAREN
	| direct_abstract_declarator LPAREN RPAREN
	| direct_abstract_declarator LPAREN parameter_type_list RPAREN
	;

designator
	: LSBRACKET constant_expression RSBRACKET
	| POINT IDENTIFIER
	;

exclusive_or_expression
	: and_expression
	| exclusive_or_expression BXOR and_expression
	;

and_expression
	: equality_expression
	| and_expression BAND equality_expression
	;

equality_expression
	: relational_expression
	| equality_expression EQUAL relational_expression
	| equality_expression NEQUAL relational_expression
	;

relational_expression
	: shift_expression
	| relational_expression LESS shift_expression
	| relational_expression GREATER shift_expression
	| relational_expression LEQUAL shift_expression
	| relational_expression GEQUAL shift_expression
	;

shift_expression
	: additive_expression
	| shift_expression LSHIFT additive_expression
	| shift_expression RSHIFT additive_expression
	;

additive_expression
	: multiplicative_expression
	| additive_expression PLUS multiplicative_expression
	| additive_expression MINUS multiplicative_expression
	;

multiplicative_expression
	: cast_expression
	| multiplicative_expression ASTERISK cast_expression
	| multiplicative_expression SLASH cast_expression
	| multiplicative_expression PERCENT cast_expression
	;

struct_or_union_specifier
	: struct_or_union LBRACE struct_declaration_list RBRACE
	| struct_or_union IDENTIFIER LBRACE struct_declaration_list RBRACE
	| struct_or_union IDENTIFIER
	;

struct_or_union
	: STRUCT
	| UNION
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: specifier_qualifier_list SEMICOLON	/* for anonymous struct/union */
	| specifier_qualifier_list struct_declarator_list SEMICOLON
	;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list COMMA struct_declarator
	;

struct_declarator
	: COLON constant_expression
	| declarator COLON constant_expression
	| declarator
	;

enum_specifier
	: ENUM LBRACE enumerator_list RBRACE
	| ENUM LBRACE enumerator_list COMMA RBRACE
	| ENUM IDENTIFIER LBRACE enumerator_list RBRACE
	| ENUM IDENTIFIER LBRACE enumerator_list COMMA RBRACE
	| ENUM IDENTIFIER
	;

enumerator_list
	: enumerator
	| enumerator_list COMMA enumerator
	;

enumerator	/* identifiers must be flagged as ENUMERATION_CONSTANT */
	: IDENTIFIER ASSIGNMENT constant_expression
	| IDENTIFIER
	;

%%

void parsing::Parser::error(const std::string& msg) {
    std::cerr << "Line " << scanner->lineno() << ": " << msg << '\n';
}
