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
  til::block_node      *block;            /* block of instructions */
};

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING

%token tINT tDOUB tSTR tVOID

%token tGE tLE tEQ tNE tAND tOR

%token tEXTERNAL tFORWARD tPUBLIC tVAR tPRIVATE

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

%type <node> program declaration instruction
%type <sequence> exprs types instructions declarations
%type <expression> expr
%type <type> type non_void_type function_type
%type <lvalue> lval
%type <block> declarations_instructions block
%type <i> qualifier

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

program : '(' declarations_instructions ')' { compiler->ast(new til::program_node(LINE, $2)); }
        ;

declarations_instructions: declarations instructions   { $$ = new til::block_node(LINE, $1, $2); }
                         | declarations                { $$ = new til::block_node(LINE, $1 , new cdk::sequence_node(LINE)); }
                         | instructions                { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), $1); }
                         |                             { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
                         ;

type : non_void_type  { $$ = $1; }
     | tVOID          { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
     ;

non_void_type : tINT    { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
              | tDOUB   { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
              | tSTR    { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
              | type '!' { $$ = cdk::reference_type::create(4, $1); }
          /*? | function_type { $$ = $1; }*/
              ;

/* This is a tricky one. Create function is defined as follows:
create (const std::vector< std::shared_ptr< basic_type > > &input_types, const std::shared_ptr< basic_type > &output_type)

Therefore, the function types must be converted to a vector of types.

function_type  : '(' type ')' { $$ = cdk::functional_type::create($2); }
               | '(' type '(' types ')' ')' { $$ = cdk::functional_type::create($4, $2); }
               ;
*/

types : type { $$ = new cdk::sequence_node(LINE, $1); }
      | type types { $$ = new cdk::sequence_node(LINE, $1, $2); }
      ;

block : '(' tBLOCK declarations_instructions')' { $$ = $3; }
      ;

qualifier : tPUBLIC      { $$ = tPUBLIC; }
          | tEXTERNAL    { $$ = tEXTERNAL; }
          | tFORWARD     { $$ = tFORWARD; }
          ;

declarations : declaration declarations { $$ = new cdk::sequence_node(LINE, $1, $2); }
             | declaration { $$ = new cdk::sequence_node(LINE, $1); }
             ;

/*? Maybe needs * in identifiers $$ */
declaration  : type tIDENTIFIER { $$ = new til::var_declaration_node(LINE, tPRIVATE, *$2, nullptr, $1); }
             | qualifier type tIDENTIFIER { $$ = new til::var_declaration_node(LINE, $1, *$3, nullptr ,$2); }
             | qualifier type tIDENTIFIER expr { $$ = new til::var_declaration_node(LINE, $1, *$3, $4 ,nullptr); }
             | type tIDENTIFIER expr { $$ = new til::var_declaration_node(LINE, tPRIVATE, *$2, $3, $1); }
             | tVAR tIDENTIFIER expr { $$ = new til::var_declaration_node(LINE, tPRIVATE ,*$2 , $3, nullptr); }
             | qualifier tVAR tIDENTIFIER expr { $$ = new til::var_declaration_node(LINE, $1 , *$3, $4, nullptr); }

instructions: instruction instructions  { $$ = new cdk::sequence_node(LINE, $1, $2); }
            | instruction               { $$ = new cdk::sequence_node(LINE, $1); }
            ;

instruction : '(' expr ')'                             {$$ = new til::evaluation_node(LINE, $2); }
            | '(' tPRINT exprs ')'                     {$$ = new til::print_node(LINE, $3, false); }
            | '(' tPRINTLN exprs ')'                   {$$ = new til::print_node(LINE, $3, true); }
            | '(' tSTOP tINTEGER ')'                   {$$ = new til::stop_node(LINE, $3); }
            | '(' tSTOP ')'                            {$$ = new til::stop_node(LINE); }
            | '(' tNEXT tINTEGER ')'                   {$$ = new til::next_node(LINE, $3); }
            | '(' tNEXT ')'                            {$$ = new til::next_node(LINE); }
            | '(' tRETURN expr ')'                     {$$ = new til::return_node(LINE, $3); }
            | '(' tRETURN ')'                          {$$ = new til::return_node(LINE, nullptr); }
            | '(' tIF expr instruction ')'             {$$ = new til::if_node(LINE, $3, $4); }
            | '(' tIF expr instruction instruction ')' {$$ = new til::if_else_node(LINE, $3, $4, $5); }
            | '(' tWHILE expr instruction ')'          {$$ = new til::loop_node(LINE, $3, $4); }
            | block                                    {$$ = $1; }

exprs : expr                      { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs expr                { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

expr : tINTEGER              { $$ = new cdk::integer_node(LINE, $1); }
     | tSTRING               { $$ = new cdk::string_node(LINE, $1); }
     | tDOUBLE               { $$ = new cdk::double_node(LINE, $1); }
     | tNULL                 { $$ = new til::nullptr_node(LINE); }
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
     | tINDEX expr expr ')' { $$ = new til::index_node(LINE, $2, $3); }
     ;

%%
