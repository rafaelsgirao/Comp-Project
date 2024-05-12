%option c++ prefix="til_scanner_" outfile="til_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "til_parser.tab.h"

// don't change this
#define yyerror LexerError

bool convert_to_integer(int &val, const std::string &s, int base) {
  try {
    val = std::strtol(s.c_str(), nullptr, base);
    return true;
  } catch (std::invalid_argument &e) {
    return false;
  } catch (std::out_of_range &e) {
    return false;
  }
}

bool convert_to_double(double &val, const std::string &s) {
  try {
    val = std::stod(s);
    return true;
  } catch (std::invalid_argument &e) {
    return false;
  } catch (std::out_of_range &e) {
    return false;
  }
}
%}
%x X_STRING X_COMMENT
%%
  yydebug=1; set_debug(1);

 /* Comments.*/
";".*                 ; /* ignore single-line comments. */
"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"/*"        yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>.|\n        ; 

">="                   return tGE; 
"<="                   return tLE; 
"=="                   return tEQ; 
"!="                   return tNE; 
"&&"                   return tAND;
"||"                   return tOR;

"int"                  return tINT;
"double"               return tDOUB;
"string"               return tSTR;
"void"                 return tVOID;

"external"             return tEXTERNAL;
"forward"              return tFORWARD;
"public"               return tPUBLIC;
"var"                  return tVAR;

"block"                return tBLOCK;
"if"                   return tIF; 
"loop"                 return tWHILE; 
"stop"                 return tSTOP;
"next"                 return tNEXT;
"return"               return tRETURN;
"print"                return tPRINT;
"println"              return tPRINTLN;

"read"                 return tREAD; 
"null"                 return tNULL;
"set"                  return tSET;
"index"                return tINDEX;
"objects"              return tOBJECTS;
"sizeof"               return tSIZEOF;
"function"             return tFUNCTION;

"program"              return tPROGRAM;

[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;

\"                     yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\"           yy_pop_state(); return tSTRING;
<X_STRING>\\\"         *yylval.s += yytext + 1;
<X_STRING>.            *yylval.s += yytext;
<X_STRING>\\0          yyerror("null character in string");       
<X_STRING>\n           yyerror("newline in string");

([1-9][0-9]*)|0                  if (!convert_to_integer(yylval.i ,yytext, 10)) yyerror("Value out of range"); return tINTEGER;
0x[0-9a-fA-F]+                   if (!convert_to_integer(yylval.i ,yytext, 16)) yyerror("Value out of range"); return tINTEGER;

[0-9]+.[0-9]+([eE][-]?[0-9]+)?   if (!convert_to_double(yylval.d, yytext)) yyerror("Value out of range"); return tDOUBLE;
[0-9]+[eE][-]?[0-9]+             if (!convert_to_double(yylval.d, yytext)) yyerror("Value out of range"); return tDOUBLE;

[-()<>=+*/%;{}.]       return *yytext;

[ \t\n]+               ; /* ignore whitespace */

@                      return tRECURSION;

.                      yyerror("Unknown character");

%%
