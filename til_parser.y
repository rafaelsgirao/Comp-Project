%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;          /* integer value */
  double                d;          /* double value */
  std::string          *s;          /* symbol name or string literal */
  cdk::basic_node      *node;       /* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;
};

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING

%token tINT tDOUB tSTR tVOID

%token tGE tLE tEQ tNE tAND tOR

%token tEXTERNAL tFORWARD tPUBLIC tVAR

%token tBLOCK tIF tWHILE tSTOP tNEXT tRETURN tPRINT tPRINTLN

%token tREAD tNULL tSET tINDEX tOBJECTS tSIZEOF tFUNCTION 

%token tPROGRAM

%token tBEGIN tEND /*not sure if these are needed*/


%nonassoc tIFX

%right '='
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY

%type <node> stmt program
%type <sequence> list
%type <expression> expr
%type <lvalue> lval

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

declarations_instructions: list exprs { $$ = new til::block_node(LINE, $1, $2); }
                        | list       { $$ = new til::block_node(LINE, $1 , new cdk::sequence_node(LINE)); }
                        | exprs      { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), $1); }
                        |            { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
                        ;

program : '(' declarations_instructions ')' { compiler->ast(new til::program_node(LINE, $2)); }
        ;

list : stmt      { $$ = new cdk::sequence_node(LINE, $1); }
     | list stmt { $$ = new cdk::sequence_node(LINE, $2, $1); }
     ;

type : non_void_type  { $$ = $1; }
     | tVOID          { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
     ;

non_void_type : tINT    { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
              | tDOUB   { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
              | tSTR    { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
              | type '!' { $$ = cdk::pointer_type::create(4, $2); }
              | function_type { $$ = $1; }

function_type  : '(' type ')' { $$ = cdk::functional_type::create($2); }
               | '(' type '(' types ')' ')' { $$ = cdk::functional_type::create($2, $4); }
               ;

types : type { $$ = new cdk::sequence_node(LINE, $1); }
      | type types { $$ = new cdk::sequence_node(LINE, $1, $2); }
      ;

block : '(' tBLOCK declarations_instructions')' { $$ = new til::block_node(LINE); }
      ;

stmt : expr ';'                         { $$ = new til::evaluation_node(LINE, $1); }
     | tPRINT exprs ';'                  { $$ = new til::print_node(LINE, $2, false); }
     | tPRINTLN exprs ';'                { $$ = new til::print_node(LINE, $2, true); }
     | tREAD ';'                        { $$ = new til::read_node(LINE); }
     | tWHILE '(' expr ')' stmt         { $$ = new til::loop_node(LINE, $3, $5); }
     | tIF expr stmt %prec tIFX         { $$ = new til::if_node(LINE, $2, $3); }
     | tIF expr stmt stmt               { $$ = new til::if_else_node(LINE, $2, $3, $4); }
     | '{' list '}'                     { $$ = $2; }
     | tSTOP tINTEGER ';'               { $$ = new til::stop_node(LINE, $2); }
     | tSTOP ';'                        { $$ = new til::stop_node(LINE); }
     | tNEXT tINTEGER ';'               { $$ = new til::next_node(LINE, $2); }
     | tNEXT ';'                        { $$ = new til::next_node(LINE); }
     | tRETURN expr ';'                 { $$ = new til::return_node(LINE, $2); }
     | tRETURN ';'                      { $$ = new til::return_node(LINE, nullptr); }
     ;

exprs : expr                      { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs expr                { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

expr : tINTEGER              { $$ = new cdk::integer_node(LINE, $1); }
     | tSTRING               { $$ = new cdk::string_node(LINE, $1); }
     | tDOUBLE               { $$ = new cdk::double_node(LINE, $1); }
     | tNULL                 { $$ = new cdk::null_node(LINE); }
     | '-' expr %prec tUNARY { $$ = new cdk::unary_minus_node(LINE, $2); }
     | '+' expr %prec tUNARY { $$ = new cdk::unary_plus_node(LINE, $2); }
     | expr '+' expr         { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr         { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr         { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr         { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr         { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr         { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr         { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr         { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr         { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr         { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr         { $$ = new cdk::eq_node(LINE, $1, $3); }
     | '(' expr ')'          { $$ = $2; }
     | lval                  { $$ = new cdk::rvalue_node(LINE, $1); }
     | lval '=' expr         { $$ = new cdk::assignment_node(LINE, $1, $3); }
     ;

lval : tIDENTIFIER             { $$ = new cdk::variable_node(LINE, $1); }
     ;

%%
