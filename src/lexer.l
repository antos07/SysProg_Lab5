%{
#include "Parser.hpp"
#include "Scanner.hpp"
#define YY_DECL int calc::Scanner::lex(calc::Parser::semantic_type *yylval)
#define YYERROR_VERBOSE 1
%}

%option c++ noyywrap yylineno outfile="Scanner.cpp"

identifier              ([[a-zA-Z_]\w*)
string                  (\"[^\n"]*\")
integer_constant        ((?:[1-9]\d*|0[xX][\da-fA-F]+|0[0-7]*)(?:[uU][lL]?|[lL][uU]?)?)
character_constant      (\'(?:\w|\\\S+)\')
floating_constant       ((?:\d*\.\d+(?:[eE][+-]?\d+)?|\d+\.|\d+[eE][+-]?\d+)(?:[uU][lL]?|[lL][uU]?)?)

%%

","                     { return Parser::token::COMMA; }
":"                     { return Parser::token::COLON; }
";"                     { return Parser::token::SEMICOLON; }
"{"                     { return Parser::token::LBRACE; }
"}"                     { return Parser::token::RBRACE; }
"("                     { return Parser::token::LPAREN; }
")"                     { return Parser::token::RPAREN; }
"["                     { return Parser::token::LSBRACKET; }
"]"                     { return Parser::token::RSBRACKET; }
"?"                     { return Parser::token::QUESTION; }
"."                     { return Parser::token::POINT; }
"->"                    { return Parser::token::ARROW; }
"..."                   { return Parser::token::ELLIPSIS; }

"+"                     { return Parser::token::PLUS; }
"-"                     { return Parser::token::MINUS; }
"*"                     { return Parser::token::ASTERISK; }
"/"                     { return Parser::token::SLASH; }
"%"                     { return Parser::token::PERCENT; }

"++"                    { return Parser::token::INCREMENT; }
"--"                    { return Parser::token::DECREMENT; }

"!"                     { return Parser::token::LNOT; }
"||"                    { return Parser::token::LOR; }
"&&"                    { return Parser::token::LAND; }
"=="                    { return Parser::token::EQUAL; }
"!="                    { return Parser::token::NEQUAL; }
"<"                     { return Parser::token::LESS; }
">"                     { return Parser::token::GREATER; }
"<="                    { return Parser::token::LEQUAL; }
">="                    { return Parser::token::GEQUAL; }

"|"                     { return Parser::token::BOR; }
"&"                     { return Parser::token::BAND; }
"^"                     { return Parser::token::BXOR; }
"~"                     { return Parser::token::BINV; }
"<<"                    { return Parser::token::LSHIFT; }
">>"                    { return Parser::token::RSHIFT; }

"="                     { return Parser::token::ASSIGNMENT; }
"*="				    { return Parser::token::MUL_ASSIGNMENT; }
"/="				    { return Parser::token::DIV_ASSIGNMENT; }
"%="				    { return Parser::token::MOD_ASSIGNMENT; }
"+="				    { return Parser::token::PLUS_ASSIGNMENT; }
"-="				    { return Parser::token::MINUS_ASSIGNMENT; }
"<<="				    { return Parser::token::LSHIFT_ASSIGNMENT; }
">>="				    { return Parser::token::RSHIFT_ASSIGNMENT; }
"&="				    { return Parser::token::BAND_ASSIGNMENT; }
"|="				    { return Parser::token::BOR_ASSIGNMENT; }
"^="				    { return Parser::token::BXOR_ASSIGNMENT; }

"sizeof"                { return Parser::token::SIZEOF; }

"void"                  { return Parser::token::VOID; }
"char"                  { return Parser::token::CHAR; }
"short"                 { return Parser::token::SHORT; }
"int"                   { return Parser::token::INT; }
"long"                  { return Parser::token::LONG; }
"float"                 { return Parser::token::FLOAT; }
"double"                { return Parser::token::DOUBLE; }
"signed"                { return Parser::token::SIGNED; }
"unsigned"              { return Parser::token::UNSIGNED; }

"auto"                  { return Parser::token::AUTO; }
"register"              { return Parser::token::REGISTER; }
"static"                { return Parser::token::STATIC; }
"extern"                { return Parser::token::EXTERN; }
"typedef"               { return Parser::token::TYPEDEF; }

"const"                 { return Parser::token::CONST; }
"volatile"              { return Parser::token::VOLATILE; }

"enum"                  { return Parser::token::ENUM; }
"struct"                { return Parser::token::STRUCT; }
"union"                 { return Parser::token::UNION; }
"case"                  { return Parser::token::CASE; }
"default"               { return Parser::token::DEFAULT; }
"if"                    { return Parser::token::IF; }
"else"                  { return Parser::token::ELSE; }
"switch"                { return Parser::token::SWITCH; }
"while"				    { return Parser::token::WHILE; }
"do"				    { return Parser::token::DO; }
"for"				    { return Parser::token::FOR; }
"goto"				    { return Parser::token::GOTO; }
"continue"			    { return Parser::token::CONTINUE; }
"break"				    { return Parser::token::BREAK; }
"return"			    { return Parser::token::RETURN; }

{identifier}            { yylval->emplace<std::string>(YYText()); return Parser::token::IDENTIFIER; }
{string}                { yylval->emplace<std::string>(YYText()); return Parser::token::STRING; }
{integer_constant}      { yylval->emplace<std::string>(YYText()); return Parser::token::INTEGER_CONSTANT; }
{character_constant}    { yylval->emplace<std::string>(YYText()); return Parser::token::CHARACTER_CONSTANT; }
{floating_constant}     { yylval->emplace<std::string>(YYText()); return Parser::token::FLOATING_CONSTANT; }
<<EOF>>                 { return Parser::token::YYEOF; }
[[:space:]]             /* no action for whitespaces */

%%

int yyFlexLexer::yylex() {
    throw std::runtime_error("Bad call to yyFlexLexer::yylex()");
}
