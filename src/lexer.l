%{
#include "Parser.hpp"
#include "Scanner.hpp"
#define YY_DECL int calc::Scanner::lex(calc::Parser::semantic_type *yylval)
#define YYERROR_VERBOSE 1
%}

%option c++ noyywrap yylineno outfile="Scanner.cpp"

identifier        ([[a-zA-Z_]\w*)

%%

";"               { return Parser::token::SEMICOLON; }

{identifier}      { yylval->emplace<std::string>(YYText()); return Parser::token::IDENTIFIER; }

"void"            { return Parser::token::VOID; }
"char"            { return Parser::token::CHAR; }
"short"           { return Parser::token::SHORT; }
"int"             { return Parser::token::INT; }
"long"            { return Parser::token::LONG; }
"float"           { return Parser::token::FLOAT; }
"double"          { return Parser::token::DOUBLE; }
"signed"          { return Parser::token::SIGNED; }
"unsigned"        { return Parser::token::UNSIGNED; }

<<EOF>>           { return Parser::token::YYEOF; }
[[:space:]]       /* no action for whitespaces */

%%

int yyFlexLexer::yylex() {
    throw std::runtime_error("Bad call to yyFlexLexer::yylex()");
}
