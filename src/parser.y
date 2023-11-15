%{
#include <iostream>
#include <string>
#include <cmath>
#include <string>
#include <memory>
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
%parse-param {Scanner* scanner} {std::unique_ptr<ast::TreeNode> &ast_root}

%code requires
{
    #include <memory>
    #include "ast/TreeNode.hpp"

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

%type <std::unique_ptr<ast::TreeNode>> external_declaration_list
%type <std::unique_ptr<ast::TreeNode>> external_declaration
%type <std::unique_ptr<ast::TreeNode>> function_definition
%type <std::unique_ptr<ast::TreeNode>> declaration
%type <std::unique_ptr<ast::TreeNode>> declaration_specifiers
%type <std::unique_ptr<ast::TreeNode>> declarator
%type <std::unique_ptr<ast::TreeNode>> declaration_list
%type <std::unique_ptr<ast::TreeNode>> compound_statement
%type <std::unique_ptr<ast::TreeNode>> init_declarator_list
%type <std::unique_ptr<ast::TreeNode>> storage_class_specifier
%type <std::unique_ptr<ast::TreeNode>> type_specifier
%type <std::unique_ptr<ast::TreeNode>> type_qualifier
%type <std::unique_ptr<ast::TreeNode>> function_specifier
%type <std::unique_ptr<ast::TreeNode>> pointer
%type <std::unique_ptr<ast::TreeNode>> direct_declarator
%type <std::unique_ptr<ast::TreeNode>> block_item_list
%type <std::unique_ptr<ast::TreeNode>> init_declarator
%type <std::unique_ptr<ast::TreeNode>> type_qualifier_list
%type <std::unique_ptr<ast::TreeNode>> assignment_expression
%type <std::unique_ptr<ast::TreeNode>> parameter_type_list
%type <std::unique_ptr<ast::TreeNode>> identifier_list
%type <std::unique_ptr<ast::TreeNode>> block_item
%type <std::unique_ptr<ast::TreeNode>> initializer
%type <std::unique_ptr<ast::TreeNode>> conditional_expression
%type <std::unique_ptr<ast::TreeNode>> unary_expression
%type <std::unique_ptr<ast::TreeNode>> assignment_operator
%type <std::unique_ptr<ast::TreeNode>> parameter_list
%type <std::unique_ptr<ast::TreeNode>> statement
%type <std::unique_ptr<ast::TreeNode>> initializer_list
%type <std::unique_ptr<ast::TreeNode>> logical_or_expression
%type <std::unique_ptr<ast::TreeNode>> postfix_expression
%type <std::unique_ptr<ast::TreeNode>> unary_operator
%type <std::unique_ptr<ast::TreeNode>> cast_expression
%type <std::unique_ptr<ast::TreeNode>> type_name
%type <std::unique_ptr<ast::TreeNode>> parameter_declaration
%type <std::unique_ptr<ast::TreeNode>> labeled_statement
%type <std::unique_ptr<ast::TreeNode>> expression_statement
%type <std::unique_ptr<ast::TreeNode>> selection_statement
%type <std::unique_ptr<ast::TreeNode>> iteration_statement
%type <std::unique_ptr<ast::TreeNode>> jump_statement
%type <std::unique_ptr<ast::TreeNode>> designation
%type <std::unique_ptr<ast::TreeNode>> logical_and_expression
%type <std::unique_ptr<ast::TreeNode>> primary_expression
%type <std::unique_ptr<ast::TreeNode>> expression
%type <std::unique_ptr<ast::TreeNode>> argument_expression_list
%type <std::unique_ptr<ast::TreeNode>> specifier_qualifier_list
%type <std::unique_ptr<ast::TreeNode>> abstract_declarator
%type <std::unique_ptr<ast::TreeNode>> constant_expression
%type <std::unique_ptr<ast::TreeNode>> designator_list
%type <std::unique_ptr<ast::TreeNode>> inclusive_or_expression
%type <std::unique_ptr<ast::TreeNode>> constant
%type <std::unique_ptr<ast::TreeNode>> direct_abstract_declarator
%type <std::unique_ptr<ast::TreeNode>> designator
%type <std::unique_ptr<ast::TreeNode>> exclusive_or_expression
%type <std::unique_ptr<ast::TreeNode>> and_expression
%type <std::unique_ptr<ast::TreeNode>> equality_expression
%type <std::unique_ptr<ast::TreeNode>> relational_expression
%type <std::unique_ptr<ast::TreeNode>> shift_expression
%type <std::unique_ptr<ast::TreeNode>> additive_expression
%type <std::unique_ptr<ast::TreeNode>> multiplicative_expression
%type <std::unique_ptr<ast::TreeNode>> struct_or_union_specifier
%type <std::unique_ptr<ast::TreeNode>> struct_or_union
%type <std::unique_ptr<ast::TreeNode>> struct_declaration_list
%type <std::unique_ptr<ast::TreeNode>> struct_declaration
%type <std::unique_ptr<ast::TreeNode>> struct_declarator_list
%type <std::unique_ptr<ast::TreeNode>> struct_declarator
%type <std::unique_ptr<ast::TreeNode>> enum_specifier
%type <std::unique_ptr<ast::TreeNode>> enumerator_list
%type <std::unique_ptr<ast::TreeNode>> enumerator

%%

translation_unit
	: external_declaration_list {
	    ast_root = std::move($1);
	}
	;

external_declaration_list
    : %empty  {
        $$ = ast::make_node("translation_unit");
    }
    | external_declaration_list external_declaration {
        $$ = ast::make_node("translation_unit");
        $$->append_child(std::move($1));
        $$->append_child(std::move($2));
    }
    ;

external_declaration
	: function_definition {
        $$ = ast::make_node("external_declaration");
        $$->append_child(std::move($1));
    }
	| declaration {
        $$ = ast::make_node("external_declaration");
        $$->append_child(std::move($1));
    }
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement {
       $$ = ast::make_node("function_definition");
       $$->append_child(std::move($1));
       $$->append_child(std::move($2));
       $$->append_child(std::move($3));
       $$->append_child(std::move($4));
    }
	| declaration_specifiers declarator compound_statement {
        $$ = ast::make_node("function_definition");
        $$->append_child(std::move($1));
        $$->append_child(std::move($2));
        $$->append_child(std::move($3));
    }
	;

declaration
	: declaration_specifiers SEMICOLON {
	    $$ = ast::make_node("declaration");
	    $$->append_child(std::move($1));
		$$->append_child(ast::make_node(";"));
	}
	| declaration_specifiers init_declarator_list SEMICOLON {
		$$ = ast::make_node("declaration");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node(";"));
	}
	;

declaration_specifiers
	: storage_class_specifier declaration_specifiers {
		$$ = ast::make_node("declaration_specifiers");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| storage_class_specifier {
		$$ = ast::make_node("declaration_specifiers");
		$$->append_child(std::move($1));
	}
	| type_specifier declaration_specifiers {
		$$ = ast::make_node("declaration_specifiers");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| type_specifier {
		$$ = ast::make_node("declaration_specifiers");
		$$->append_child(std::move($1));
	}
	| type_qualifier declaration_specifiers {
		$$ = ast::make_node("declaration_specifiers");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| type_qualifier {
		$$ = ast::make_node("declaration_specifiers");
		$$->append_child(std::move($1));
	}
	| function_specifier declaration_specifiers {
		$$ = ast::make_node("declaration_specifiers");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| function_specifier {
		$$ = ast::make_node("declaration_specifiers");
		$$->append_child(std::move($1));
	}
	;

declarator
	: pointer direct_declarator {
		$$ = ast::make_node("declarator");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| direct_declarator {
		$$ = ast::make_node("declarator");
		$$->append_child(std::move($1));
	}
	;

declaration_list
	: declaration {
		$$ = ast::make_node("declaration_list");
		$$->append_child(std::move($1));
	}
	| declaration_list declaration {
		$$ = ast::make_node("declaration_list");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	;

compound_statement
	: LBRACE RBRACE {
		$$ = ast::make_node("compound_statement");
		$$->append_child(ast::make_node("{"));
		$$->append_child(ast::make_node("}"));
	}
	| LBRACE block_item_list RBRACE {
		$$ = ast::make_node("compound_statement");
		$$->append_child(ast::make_node("{"));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node("}"));
	}
	;

init_declarator_list
	: init_declarator {
		$$ = ast::make_node("init_declarator_list");
		$$->append_child(std::move($1));
	}
	| init_declarator_list COMMA init_declarator {
		$$ = ast::make_node("init_declarator_list");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(","));
		$$->append_child(std::move($3));
	}
	;

storage_class_specifier
	: TYPEDEF {
		$$ = ast::make_node("storage_class_specifier");
		$$->append_child(ast::make_node("typedef"));
	}
	| EXTERN {
		$$ = ast::make_node("storage_class_specifier");
		$$->append_child(ast::make_node("extern"));
	}
	| STATIC {
		$$ = ast::make_node("storage_class_specifier");
		$$->append_child(ast::make_node("static"));
	}
	| AUTO {
		$$ = ast::make_node("storage_class_specifier");
		$$->append_child(ast::make_node("auto"));
	}
	| REGISTER {
		$$ = ast::make_node("storage_class_specifier");
		$$->append_child(ast::make_node("register"));
	}
	;

type_specifier
	: VOID {
		$$ = ast::make_node("type_specifier");
		$$->append_child(ast::make_node("void"));
	}
	| CHAR {
		$$ = ast::make_node("type_specifier");
		$$->append_child(ast::make_node("char"));
	}
	| SHORT {
		$$ = ast::make_node("type_specifier");
		$$->append_child(ast::make_node("short"));
	}
	| INT {
		$$ = ast::make_node("type_specifier");
		$$->append_child(ast::make_node("int"));
	}
	| LONG {
		$$ = ast::make_node("type_specifier");
		$$->append_child(ast::make_node("long"));
	}
	| FLOAT {
		$$ = ast::make_node("type_specifier");
		$$->append_child(ast::make_node("float"));
	}
	| DOUBLE {
		$$ = ast::make_node("type_specifier");
		$$->append_child(ast::make_node("double"));
	}
	| SIGNED {
		$$ = ast::make_node("type_specifier");
		$$->append_child(ast::make_node("signed"));
	}
	| UNSIGNED {
		$$ = ast::make_node("type_specifier");
		$$->append_child(ast::make_node("unsigned"));
	}
	| BOOL {
		$$ = ast::make_node("type_specifier");
		$$->append_child(ast::make_node("bool"));
	}
	| struct_or_union_specifier {
		$$ = ast::make_node("type_specifier");
		$$->append_child(std::move($1));
	}
	| enum_specifier {
		$$ = ast::make_node("type_specifier");
		$$->append_child(std::move($1));
	}
	/* | IDENTIFIER	—  removed support for typedefs*/
	;

type_qualifier
	: CONST {
		$$ = ast::make_node("type_qualifier");
		$$->append_child(ast::make_node("const"));
	}
	| RESTRICT {
		$$ = ast::make_node("type_qualifier");
		$$->append_child(ast::make_node("restrict"));
	}
	| VOLATILE {
		$$ = ast::make_node("type_qualifier");
		$$->append_child(ast::make_node("volatile"));
	}
	;

function_specifier
	: INLINE {
		$$ = ast::make_node("function_specifier");
		$$->append_child(ast::make_node("inline"));
	}
	;

pointer
	: ASTERISK type_qualifier_list pointer {
		$$ = ast::make_node("pointer");
		$$->append_child(ast::make_node("*"));
		$$->append_child(std::move($2));
		$$->append_child(std::move($3));
	}
	| ASTERISK type_qualifier_list {
		$$ = ast::make_node("pointer");
		$$->append_child(ast::make_node("*"));
		$$->append_child(std::move($2));
	}
	| ASTERISK pointer {
		$$ = ast::make_node("pointer");
		$$->append_child(ast::make_node("*"));
		$$->append_child(std::move($2));
	}
	| ASTERISK {
		$$ = ast::make_node("pointer");
		$$->append_child(ast::make_node("*"));
	}
	;

direct_declarator
	: IDENTIFIER {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(ast::make_node(std::move($1)));
	}
	| LPAREN declarator RPAREN {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node(")"));
	}
	| direct_declarator LSBRACKET RSBRACKET {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(ast::make_node("]"));
	}
	| direct_declarator LSBRACKET ASTERISK RSBRACKET {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(ast::make_node("*"));
		$$->append_child(ast::make_node("]"));
	}
	| direct_declarator LSBRACKET STATIC type_qualifier_list assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(ast::make_node("static"));
		$$->append_child(std::move($4));
		$$->append_child(std::move($5));
		$$->append_child(ast::make_node("]"));
	}
	| direct_declarator LSBRACKET STATIC assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(ast::make_node("static"));
		$$->append_child(std::move($4));
		$$->append_child(ast::make_node("]"));
	}
	| direct_declarator LSBRACKET type_qualifier_list ASTERISK RSBRACKET {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node("*"));
		$$->append_child(ast::make_node("]"));
	}
	| direct_declarator LSBRACKET type_qualifier_list STATIC assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node("static"));
		$$->append_child(std::move($5));
		$$->append_child(ast::make_node("]"));
	}
	| direct_declarator LSBRACKET type_qualifier_list assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($3));
		$$->append_child(std::move($4));
		$$->append_child(ast::make_node("]"));
	}
	| direct_declarator LSBRACKET type_qualifier_list RSBRACKET {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node("]"));
	}
	| direct_declarator LSBRACKET assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node("]"));
	}
	| direct_declarator LPAREN parameter_type_list RPAREN {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node(")"));
	}
	| direct_declarator LPAREN RPAREN {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("("));
		$$->append_child(ast::make_node(")"));
	}
	| direct_declarator LPAREN identifier_list RPAREN {
		$$ = ast::make_node("direct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node(")"));
	}
	;

block_item_list
	: block_item {
		$$ = ast::make_node("block_item_list");
		$$->append_child(std::move($1));
	}
	| block_item_list block_item {
		$$ = ast::make_node("block_item_list");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	;

init_declarator
	: declarator ASSIGNMENT initializer {
		$$ = ast::make_node("init_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("="));
		$$->append_child(std::move($3));
	}
	| declarator {
		$$ = ast::make_node("init_declarator");
		$$->append_child(std::move($1));
	}
	;

type_qualifier_list
	: type_qualifier {
		$$ = ast::make_node("type_qualifier_list");
		$$->append_child(std::move($1));
	}
	| type_qualifier_list type_qualifier {
		$$ = ast::make_node("type_qualifier_list");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	;

assignment_expression
	: conditional_expression {
		$$ = ast::make_node("assignment_expression");
		$$->append_child(std::move($1));
	}
	| unary_expression assignment_operator assignment_expression {
		$$ = ast::make_node("assignment_expression");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
		$$->append_child(std::move($3));
	}
	;

parameter_type_list
	: parameter_list COMMA ELLIPSIS {
		$$ = ast::make_node("parameter_type_list");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(","));
		$$->append_child(ast::make_node("..."));
	}
	| parameter_list {
		$$ = ast::make_node("parameter_type_list");
		$$->append_child(std::move($1));
	}
	;

identifier_list
	: IDENTIFIER {
		$$ = ast::make_node("identifier_list");
		$$->append_child(ast::make_node(std::move($1)));
	}
	| identifier_list COMMA IDENTIFIER {
		$$ = ast::make_node("identifier_list");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(","));
		$$->append_child(ast::make_node(std::move($3)));
	}
	;

block_item
	: declaration {
		$$ = ast::make_node("block_item");
		$$->append_child(std::move($1));
	}
	| statement {
		$$ = ast::make_node("block_item");
		$$->append_child(std::move($1));
	}
	;

initializer
	: LBRACE initializer_list RBRACE {
		$$ = ast::make_node("initializer");
		$$->append_child(ast::make_node("{"));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node("}"));
	}
	| LBRACE initializer_list COMMA RBRACE {
		$$ = ast::make_node("initializer");
		$$->append_child(ast::make_node("{"));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node(","));
		$$->append_child(ast::make_node("}"));
	}
	| assignment_expression {
		$$ = ast::make_node("initializer");
		$$->append_child(std::move($1));
	}
	;

conditional_expression
	: logical_or_expression {
		$$ = ast::make_node("conditional_expression");
		$$->append_child(std::move($1));
	}
	| logical_or_expression QUESTION expression COLON conditional_expression {
		$$ = ast::make_node("conditional_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("?"));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node(":"));
		$$->append_child(std::move($5));
	}
	;

unary_expression
	: postfix_expression {
		$$ = ast::make_node("unary_expression");
		$$->append_child(std::move($1));
	}
	| INCREMENT unary_expression {
		$$ = ast::make_node("unary_expression");
		$$->append_child(ast::make_node("++"));
		$$->append_child(std::move($2));
	}
	| DECREMENT unary_expression {
		$$ = ast::make_node("unary_expression");
		$$->append_child(ast::make_node("--"));
		$$->append_child(std::move($2));
	}
	| unary_operator cast_expression {
		$$ = ast::make_node("unary_expression");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| SIZEOF unary_expression {
		$$ = ast::make_node("unary_expression");
		$$->append_child(ast::make_node("sizeof"));
		$$->append_child(std::move($2));
	}
	| SIZEOF LPAREN type_name RPAREN {
		$$ = ast::make_node("unary_expression");
		$$->append_child(ast::make_node("sizeof"));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node(")"));
	}
	;

assignment_operator
	: ASSIGNMENT {
		$$ = ast::make_node("assignment_operator");
		$$->append_child(ast::make_node("="));
	}
	| MUL_ASSIGNMENT {
		$$ = ast::make_node("assignment_operator");
		$$->append_child(ast::make_node("*="));
	}
	| DIV_ASSIGNMENT {
		$$ = ast::make_node("assignment_operator");
		$$->append_child(ast::make_node("/="));
	}
	| MOD_ASSIGNMENT {
		$$ = ast::make_node("assignment_operator");
		$$->append_child(ast::make_node("%="));
	}
	| PLUS_ASSIGNMENT {
		$$ = ast::make_node("assignment_operator");
		$$->append_child(ast::make_node("+="));
	}
	| MINUS_ASSIGNMENT {
		$$ = ast::make_node("assignment_operator");
		$$->append_child(ast::make_node("-="));
	}
	| LSHIFT_ASSIGNMENT {
		$$ = ast::make_node("assignment_operator");
		$$->append_child(ast::make_node("<<="));
	}
	| RSHIFT_ASSIGNMENT {
		$$ = ast::make_node("assignment_operator");
		$$->append_child(ast::make_node(">>="));
	}
	| BAND_ASSIGNMENT {
		$$ = ast::make_node("assignment_operator");
		$$->append_child(ast::make_node("&="));
	}
	| BOR_ASSIGNMENT {
		$$ = ast::make_node("assignment_operator");
		$$->append_child(ast::make_node("|="));
	}
	| BXOR_ASSIGNMENT {
		$$ = ast::make_node("assignment_operator");
		$$->append_child(ast::make_node("^="));
	}
	;

parameter_list
	: parameter_declaration {
		$$ = ast::make_node("parameter_list");
		$$->append_child(std::move($1));
	}
	| parameter_list COMMA parameter_declaration {
		$$ = ast::make_node("parameter_list");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(","));
		$$->append_child(std::move($3));
	}
	;

statement
	: labeled_statement {
		$$ = ast::make_node("statement");
		$$->append_child(std::move($1));
	}
	| compound_statement {
		$$ = ast::make_node("statement");
		$$->append_child(std::move($1));
	}
	| expression_statement {
		$$ = ast::make_node("statement");
		$$->append_child(std::move($1));
	}
	| selection_statement {
		$$ = ast::make_node("statement");
		$$->append_child(std::move($1));
	}
	| iteration_statement {
		$$ = ast::make_node("statement");
		$$->append_child(std::move($1));
	}
	| jump_statement {
		$$ = ast::make_node("statement");
		$$->append_child(std::move($1));
	}
	;

initializer_list
	: designation initializer {
		$$ = ast::make_node("initializer_list");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| initializer {
		$$ = ast::make_node("initializer_list");
		$$->append_child(std::move($1));
	}
	| initializer_list COMMA designation initializer {
		$$ = ast::make_node("initializer_list");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(","));
		$$->append_child(std::move($3));
		$$->append_child(std::move($4));
	}
	| initializer_list COMMA initializer {
		$$ = ast::make_node("initializer_list");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(","));
		$$->append_child(std::move($3));
	}
	;

logical_or_expression
	: logical_and_expression {
		$$ = ast::make_node("logical_or_expression");
		$$->append_child(std::move($1));
	}
	| logical_or_expression LOR logical_and_expression {
		$$ = ast::make_node("logical_or_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("||"));
		$$->append_child(std::move($3));
	}
	;

postfix_expression
	: primary_expression {
		$$ = ast::make_node("postfix_expression");
		$$->append_child(std::move($1));
	}
	| postfix_expression LSBRACKET expression RSBRACKET {
		$$ = ast::make_node("postfix_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node("]"));
	}
	| postfix_expression LPAREN RPAREN {
		$$ = ast::make_node("postfix_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("("));
		$$->append_child(ast::make_node(")"));
	}
	| postfix_expression LPAREN argument_expression_list RPAREN {
		$$ = ast::make_node("postfix_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node(")"));
	}
	| postfix_expression POINT IDENTIFIER {
		$$ = ast::make_node("postfix_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("."));
		$$->append_child(ast::make_node(std::move($3)));
	}
	| postfix_expression ARROW IDENTIFIER {
		$$ = ast::make_node("postfix_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("->"));
		$$->append_child(ast::make_node(std::move($3)));
	}
	| postfix_expression INCREMENT {
		$$ = ast::make_node("postfix_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("++"));
	}
	| postfix_expression DECREMENT {
		$$ = ast::make_node("postfix_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("--"));
	}
	| LPAREN type_name RPAREN RBRACE initializer_list RBRACE {
		$$ = ast::make_node("postfix_expression");
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node(")"));
		$$->append_child(ast::make_node("{"));
		$$->append_child(std::move($5));
		$$->append_child(ast::make_node("}"));
	}
	| LPAREN type_name RPAREN RBRACE initializer_list COMMA RBRACE {
		$$ = ast::make_node("postfix_expression");
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node(")"));
		$$->append_child(ast::make_node("{"));
		$$->append_child(std::move($5));
		$$->append_child(ast::make_node(","));
		$$->append_child(ast::make_node("}"));
	}
	;

unary_operator
	: BAND {
		$$ = ast::make_node("unary_operator");
		$$->append_child(ast::make_node("&"));
	}
	| ASTERISK {
		$$ = ast::make_node("unary_operator");
		$$->append_child(ast::make_node("*"));
	}
	| PLUS {
		$$ = ast::make_node("unary_operator");
		$$->append_child(ast::make_node("+"));
	}
	| MINUS {
		$$ = ast::make_node("unary_operator");
		$$->append_child(ast::make_node("-"));
	}
	| BINV {
		$$ = ast::make_node("unary_operator");
		$$->append_child(ast::make_node("~"));
	}
	| LNOT {
		$$ = ast::make_node("unary_operator");
		$$->append_child(ast::make_node("!"));
	}
	;

cast_expression
	: unary_expression {
		$$ = ast::make_node("cast_expression");
		$$->append_child(std::move($1));
	}
	| LPAREN type_name RPAREN cast_expression {
		$$ = ast::make_node("cast_expression");
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node(")"));
		$$->append_child(std::move($4));
	}
	;

type_name
	: specifier_qualifier_list abstract_declarator {
		$$ = ast::make_node("type_name");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| specifier_qualifier_list {
		$$ = ast::make_node("type_name");
		$$->append_child(std::move($1));
	}
	;

parameter_declaration
	: declaration_specifiers declarator {
		$$ = ast::make_node("parameter_declaration");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| declaration_specifiers abstract_declarator {
		$$ = ast::make_node("parameter_declaration");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| declaration_specifiers {
		$$ = ast::make_node("parameter_declaration");
		$$->append_child(std::move($1));
	}
	;

labeled_statement
	: IDENTIFIER COLON statement {
		$$ = ast::make_node("labeled_statement");
		$$->append_child(ast::make_node(std::move($1)));
		$$->append_child(ast::make_node(":"));
		$$->append_child(std::move($3));
	}
	| CASE constant_expression COLON statement {
		$$ = ast::make_node("labeled_statement");
		$$->append_child(ast::make_node("case"));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node(":"));
		$$->append_child(std::move($4));
	}
	| DEFAULT COLON statement {
		$$ = ast::make_node("labeled_statement");
		$$->append_child(ast::make_node("default"));
		$$->append_child(ast::make_node(":"));
		$$->append_child(std::move($3));
	}
	;

expression_statement
	: SEMICOLON {
		$$ = ast::make_node("expression_statement");
		$$->append_child(ast::make_node(";"));
	}
	| expression SEMICOLON {
		$$ = ast::make_node("expression_statement");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(";"));
	}
	;

selection_statement
	: IF LPAREN expression RPAREN statement ELSE statement {
		$$ = ast::make_node("selection_statement");
		$$->append_child(ast::make_node("if"));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node(")"));
		$$->append_child(std::move($5));
		$$->append_child(ast::make_node("else"));
		$$->append_child(std::move($7));
	}
	| IF LPAREN expression RPAREN statement {
		$$ = ast::make_node("selection_statement");
		$$->append_child(ast::make_node("if"));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node(")"));
		$$->append_child(std::move($5));
	}
	| SWITCH LPAREN expression RPAREN statement {
		$$ = ast::make_node("selection_statement");
		$$->append_child(ast::make_node("switch"));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node(")"));
		$$->append_child(std::move($5));
	}
	;

iteration_statement
	: WHILE LPAREN expression RPAREN statement {
		$$ = ast::make_node("iteration_statement");
		$$->append_child(ast::make_node("while"));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node(")"));
		$$->append_child(std::move($5));
	}
	| DO statement WHILE LPAREN expression RPAREN SEMICOLON {
		$$ = ast::make_node("iteration_statement");
		$$->append_child(ast::make_node("do"));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node("while"));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($5));
		$$->append_child(ast::make_node(")"));
		$$->append_child(ast::make_node(";"));
	}
	| FOR LPAREN expression_statement expression_statement RPAREN statement {
		$$ = ast::make_node("iteration_statement");
		$$->append_child(ast::make_node("for"));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(std::move($4));
		$$->append_child(ast::make_node(")"));
		$$->append_child(std::move($6));
	}
	| FOR LPAREN expression_statement expression_statement expression RPAREN statement {
		$$ = ast::make_node("iteration_statement");
		$$->append_child(ast::make_node("for"));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(std::move($4));
		$$->append_child(std::move($5));
		$$->append_child(ast::make_node(")"));
		$$->append_child(std::move($7));
	}
	| FOR LPAREN declaration expression_statement RPAREN statement {
		$$ = ast::make_node("iteration_statement");
		$$->append_child(ast::make_node("for"));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(std::move($4));
		$$->append_child(ast::make_node(")"));
		$$->append_child(std::move($6));
	}
	| FOR LPAREN declaration expression_statement expression RPAREN statement {
		$$ = ast::make_node("iteration_statement");
		$$->append_child(ast::make_node("for"));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(std::move($4));
		$$->append_child(std::move($5));
		$$->append_child(ast::make_node(")"));
		$$->append_child(std::move($7));
	}
	;

jump_statement
	: GOTO IDENTIFIER SEMICOLON {
		$$ = ast::make_node("jump_statement");
		$$->append_child(ast::make_node("goto"));
		$$->append_child(ast::make_node(std::move($2)));
		$$->append_child(ast::make_node(";"));
	}
	| CONTINUE SEMICOLON {
		$$ = ast::make_node("jump_statement");
		$$->append_child(ast::make_node("continue"));
		$$->append_child(ast::make_node(";"));
	}
	| BREAK SEMICOLON {
		$$ = ast::make_node("jump_statement");
		$$->append_child(ast::make_node("break"));
		$$->append_child(ast::make_node(";"));
	}
	| RETURN SEMICOLON {
		$$ = ast::make_node("jump_statement");
		$$->append_child(ast::make_node("return"));
		$$->append_child(ast::make_node(";"));
	}
	| RETURN expression SEMICOLON {
		$$ = ast::make_node("jump_statement");
		$$->append_child(ast::make_node("continue"));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node(";"));
	}
	;

designation
	: designator_list ASSIGNMENT {
		$$ = ast::make_node("designation");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("="));
	}
	;

logical_and_expression
	: inclusive_or_expression {
		$$ = ast::make_node("logical_and_expression");
		$$->append_child(std::move($1));
	}
	| logical_and_expression LAND inclusive_or_expression {
		$$ = ast::make_node("logical_and_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("&&"));
		$$->append_child(std::move($3));
	}
	;

primary_expression
	: IDENTIFIER {
		$$ = ast::make_node("primary_expression");
		$$->append_child(ast::make_node(std::move($1)));
	}
	| constant {
		$$ = ast::make_node("primary_expression");
		$$->append_child(std::move($1));
	}
	| STRING {
		$$ = ast::make_node("primary_expression");
		$$->append_child(ast::make_node(std::move($1)));
	}
	| LPAREN expression RPAREN {
		$$ = ast::make_node("primary_expression");
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node(")"));
	}
	;

expression
	: assignment_expression {
		$$ = ast::make_node("expression");
		$$->append_child(std::move($1));
	}
	| expression COMMA assignment_expression {
		$$ = ast::make_node("expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(","));
		$$->append_child(std::move($3));
	}
	;

argument_expression_list
	: assignment_expression {
		$$ = ast::make_node("argument_expression_list");
		$$->append_child(std::move($1));
	}
	| argument_expression_list COMMA assignment_expression {
		$$ = ast::make_node("argument_expression_list");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(","));
		$$->append_child(std::move($3));
	}
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list {
		$$ = ast::make_node("specifier_qualifier_list");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| type_specifier {
		$$ = ast::make_node("specifier_qualifier_list");
		$$->append_child(std::move($1));
	}
	| type_qualifier specifier_qualifier_list {
		$$ = ast::make_node("specifier_qualifier_list");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| type_qualifier {
		$$ = ast::make_node("specifier_qualifier_list");
		$$->append_child(std::move($1));
	}
	;

abstract_declarator
	: pointer direct_abstract_declarator {
		$$ = ast::make_node("abstract_declarator");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	| pointer {
		$$ = ast::make_node("abstract_declarator");
		$$->append_child(std::move($1));
	}
	| direct_abstract_declarator {
		$$ = ast::make_node("abstract_declarator");
		$$->append_child(std::move($1));
	}
	;

constant_expression
	: conditional_expression {
		$$ = ast::make_node("constant_expression");
		$$->append_child(std::move($1));
	}
	;

designator_list
	: designator {
		$$ = ast::make_node("designator_list");
		$$->append_child(std::move($1));
	}
	| designator_list designator {
		$$ = ast::make_node("designator_list");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	;

inclusive_or_expression
	: exclusive_or_expression {
		$$ = ast::make_node("inclusive_or_expression");
		$$->append_child(std::move($1));
	}
	| inclusive_or_expression BOR exclusive_or_expression {
		$$ = ast::make_node("inclusive_or_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("|"));
		$$->append_child(std::move($3));
	}
	;

constant
	: INTEGER_CONSTANT	{	/* includes character_constant */
        $$ = ast::make_node("constant");
        $$->append_child(ast::make_node(std::move($1)));
    }
	| FLOATING_CONSTANT {
        $$ = ast::make_node("constant");
        $$->append_child(ast::make_node(std::move($1)));
	}
	/* | IDENTIFIER	    remove support of enumeration constants */
	;

direct_abstract_declarator
	: LPAREN abstract_declarator RPAREN {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node(")"));
	}
	| LSBRACKET RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(ast::make_node("["));
		$$->append_child(ast::make_node("]"));
	}
	| LSBRACKET ASTERISK RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(ast::make_node("["));
		$$->append_child(ast::make_node("*"));
		$$->append_child(ast::make_node("]"));
	}
	| LSBRACKET STATIC type_qualifier_list assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(ast::make_node("["));
		$$->append_child(ast::make_node("static"));
		$$->append_child(std::move($3));
		$$->append_child(std::move($4));
		$$->append_child(ast::make_node("]"));
	}
	| LSBRACKET STATIC assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(ast::make_node("["));
		$$->append_child(ast::make_node("static"));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node("]"));
	}
	| LSBRACKET type_qualifier_list STATIC assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node("static"));
		$$->append_child(std::move($4));
		$$->append_child(ast::make_node("]"));
	}
	| LSBRACKET type_qualifier_list assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($2));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node("]"));
	}
	| LSBRACKET type_qualifier_list RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node("]"));
	}
	| LSBRACKET assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node("]"));
	}
	| direct_abstract_declarator LSBRACKET RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(ast::make_node("]"));
	}
	| direct_abstract_declarator LSBRACKET ASTERISK RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(ast::make_node("*"));
		$$->append_child(ast::make_node("]"));
	}
	| direct_abstract_declarator LSBRACKET STATIC type_qualifier_list assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(ast::make_node("static"));
		$$->append_child(std::move($4));
		$$->append_child(std::move($5));
		$$->append_child(ast::make_node("]"));
	}
	| direct_abstract_declarator LSBRACKET STATIC assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(ast::make_node("static"));
		$$->append_child(std::move($4));
		$$->append_child(ast::make_node("]"));
	}
	| direct_abstract_declarator LSBRACKET type_qualifier_list assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($3));
		$$->append_child(std::move($4));
		$$->append_child(ast::make_node("]"));
	}
	| direct_abstract_declarator LSBRACKET type_qualifier_list STATIC assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node("static"));
		$$->append_child(std::move($5));
		$$->append_child(ast::make_node("]"));
	}
	| direct_abstract_declarator LSBRACKET type_qualifier_list RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node("]"));
	}
	| direct_abstract_declarator LSBRACKET assignment_expression RSBRACKET {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node("]"));
	}
	| LPAREN RPAREN {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(ast::make_node("("));
		$$->append_child(ast::make_node(")"));
	}
	| LPAREN parameter_type_list RPAREN {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node(")"));
	}
	| direct_abstract_declarator LPAREN RPAREN {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("("));
		$$->append_child(ast::make_node(")"));
	}
	| direct_abstract_declarator LPAREN parameter_type_list RPAREN {
		$$ = ast::make_node("direct_abstract_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("("));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node(")"));
	}
	;

designator
	: LSBRACKET constant_expression RSBRACKET {
		$$ = ast::make_node("designator");
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($2));
		$$->append_child(ast::make_node("]"));
	}
	| POINT IDENTIFIER {
		$$ = ast::make_node("designator");
		$$->append_child(ast::make_node("."));
		$$->append_child(ast::make_node(std::move($2)));
	}
	;

exclusive_or_expression
	: and_expression {
		$$ = ast::make_node("exclusive_or_expression");
		$$->append_child(std::move($1));
	}
	| exclusive_or_expression BXOR and_expression {
		$$ = ast::make_node("exclusive_or_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("^"));
		$$->append_child(std::move($3));
	}
	;

and_expression
	: equality_expression {
		$$ = ast::make_node("and_expression");
		$$->append_child(std::move($1));
	}
	| and_expression BAND equality_expression {
		$$ = ast::make_node("and_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("&"));
		$$->append_child(std::move($3));
	}
	;

equality_expression
	: relational_expression {
		$$ = ast::make_node("equality_expression");
		$$->append_child(std::move($1));
	}
	| equality_expression EQUAL relational_expression {
		$$ = ast::make_node("equality_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("=="));
		$$->append_child(std::move($3));
	}
	| equality_expression NEQUAL relational_expression {
		$$ = ast::make_node("equality_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("!="));
		$$->append_child(std::move($3));
	}
	;

relational_expression
	: shift_expression {
		$$ = ast::make_node("relational_expression");
		$$->append_child(std::move($1));
	}
	| relational_expression LESS shift_expression {
		$$ = ast::make_node("relational_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("<"));
		$$->append_child(std::move($3));
	}
	| relational_expression GREATER shift_expression {
		$$ = ast::make_node("relational_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(">"));
		$$->append_child(std::move($3));
	}
	| relational_expression LEQUAL shift_expression {
		$$ = ast::make_node("relational_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("<="));
		$$->append_child(std::move($3));
	}
	| relational_expression GEQUAL shift_expression {
		$$ = ast::make_node("relational_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(">="));
		$$->append_child(std::move($3));
	}
	;

shift_expression
	: additive_expression {
		$$ = ast::make_node("shift_expression");
		$$->append_child(std::move($1));
	}
	| shift_expression LSHIFT additive_expression {
		$$ = ast::make_node("shift_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("<<"));
		$$->append_child(std::move($3));
	}
	| shift_expression RSHIFT additive_expression {
		$$ = ast::make_node("shift_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(">>"));
		$$->append_child(std::move($3));
	}
	;

additive_expression
	: multiplicative_expression {
		$$ = ast::make_node("additive_expression");
		$$->append_child(std::move($1));
	}
	| additive_expression PLUS multiplicative_expression {
		$$ = ast::make_node("additive_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("+"));
		$$->append_child(std::move($3));
	}
	| additive_expression MINUS multiplicative_expression {
		$$ = ast::make_node("additive_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("-"));
		$$->append_child(std::move($3));
	}
	;

multiplicative_expression
	: cast_expression {
		$$ = ast::make_node("multiplicative_expression");
		$$->append_child(std::move($1));
	}
	| multiplicative_expression ASTERISK cast_expression {
		$$ = ast::make_node("multiplicative_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("*"));
		$$->append_child(std::move($3));
	}
	| multiplicative_expression SLASH cast_expression {
		$$ = ast::make_node("multiplicative_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("/"));
		$$->append_child(std::move($3));
	}
	| multiplicative_expression PERCENT cast_expression {
		$$ = ast::make_node("multiplicative_expression");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("%"));
		$$->append_child(std::move($3));
	}
	;

struct_or_union_specifier
	: struct_or_union LBRACE struct_declaration_list RBRACE {
		$$ = ast::make_node("struct_or_union_specifier");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node("{"));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node("}"));
	}
	| struct_or_union IDENTIFIER LBRACE struct_declaration_list RBRACE {
		$$ = ast::make_node("struct_or_union_specifier");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(std::move($2)));
		$$->append_child(ast::make_node("{"));
		$$->append_child(std::move($4));
		$$->append_child(ast::make_node("}"));
	}
	| struct_or_union IDENTIFIER {
		$$ = ast::make_node("struct_or_union_specifier");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(std::move($2)));
	}
	;

struct_or_union
	: STRUCT {
		$$ = ast::make_node("struct_or_union");
		$$->append_child(ast::make_node("struct"));
	}
	| UNION {
		$$ = ast::make_node("struct_or_union");
		$$->append_child(ast::make_node("union"));
	}
	;

struct_declaration_list
	: struct_declaration {
		$$ = ast::make_node("struct_declaration");
		$$->append_child(std::move($1));
	}
	| struct_declaration_list struct_declaration {
		$$ = ast::make_node("struct_declaration");
		$$->append_child(std::move($1));
		$$->append_child(std::move($2));
	}
	;

struct_declaration
	: specifier_qualifier_list SEMICOLON {      /* for anonymous struct/union */
	    $$ = ast::make_node("struct_declaration");
        $$->append_child(std::move($1));
        $$->append_child(ast::make_node(";"));
	}
	| specifier_qualifier_list struct_declarator_list SEMICOLON {
	    $$ = ast::make_node("struct_declaration");
        $$->append_child(std::move($1));
        $$->append_child(std::move($2));
        $$->append_child(ast::make_node(";"));
	}
	;

struct_declarator_list
	: struct_declarator {
		$$ = ast::make_node("struct_declarator_list");
		$$->append_child(std::move($1));
	}
	| struct_declarator_list COMMA struct_declarator {
		$$ = ast::make_node("struct_declarator_list");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(","));
		$$->append_child(std::move($3));
	}
	;

struct_declarator
	: COLON constant_expression {
		$$ = ast::make_node("struct_declarator");
		$$->append_child(ast::make_node(":"));
		$$->append_child(std::move($2));
	}
	| declarator COLON constant_expression {
		$$ = ast::make_node("struct_declarator");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(":"));
		$$->append_child(std::move($3));
	}
	| declarator {
		$$ = ast::make_node("struct_declarator");
		$$->append_child(std::move($1));
	}
	;

enum_specifier
	: ENUM LBRACE enumerator_list RBRACE {
		$$ = ast::make_node("enum_specifier");
		$$->append_child(ast::make_node("enum"));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node("]"));
	}
	| ENUM LBRACE enumerator_list COMMA RBRACE {
		$$ = ast::make_node("enum_specifier");
		$$->append_child(ast::make_node("enum"));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($3));
		$$->append_child(ast::make_node(","));
		$$->append_child(ast::make_node("]"));
	}
	| ENUM IDENTIFIER LBRACE enumerator_list RBRACE {
		$$ = ast::make_node("enum_specifier");
		$$->append_child(ast::make_node("enum"));
		$$->append_child(ast::make_node(std::move($2)));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($4));
		$$->append_child(ast::make_node("]"));
	}
	| ENUM IDENTIFIER LBRACE enumerator_list COMMA RBRACE {
		$$ = ast::make_node("enum_specifier");
		$$->append_child(ast::make_node("enum"));
		$$->append_child(ast::make_node(std::move($2)));
		$$->append_child(ast::make_node("["));
		$$->append_child(std::move($4));
		$$->append_child(ast::make_node(","));
		$$->append_child(ast::make_node("]"));
	}
	| ENUM IDENTIFIER {
		$$ = ast::make_node("enum_specifier");
		$$->append_child(ast::make_node("enum"));
		$$->append_child(ast::make_node(std::move($2)));
	}
	;

enumerator_list
	: enumerator {
		$$ = ast::make_node("enumerator_list");
		$$->append_child(std::move($1));
	}
	| enumerator_list COMMA enumerator {
		$$ = ast::make_node("enumerator_list");
		$$->append_child(std::move($1));
		$$->append_child(ast::make_node(","));
		$$->append_child(std::move($3));
	}
	;

enumerator	/* identifiers must be flagged as ENUMERATION_CONSTANT */
	: IDENTIFIER ASSIGNMENT constant_expression {
		$$ = ast::make_node("enumerator");
		$$->append_child(ast::make_node(std::move($1)));
		$$->append_child(ast::make_node("="));
		$$->append_child(std::move($3));
	}
	| IDENTIFIER {
		$$ = ast::make_node("enumerator");
		$$->append_child(ast::make_node(std::move($1)));
	}
	;

%%

void parsing::Parser::error(const std::string& msg) {
    std::cerr << "Line " << scanner->lineno() << ": " << msg << '\n';
}
