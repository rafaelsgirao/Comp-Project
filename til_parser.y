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
  std::vector<std::shared_ptr<cdk::basic_type>> *t;
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

%token tREAD tNULL tSET tINDEX tOBJECTS tSIZEOF tFUNCTION tRECURSION

%token tPROGRAM

%nonassoc tIFX

%right '!'
%left tGE tLE tEQ tNE '>' '<' tAND tOR
%left '+' '-'
%left '*' '/' '%' 
%nonassoc tUNARY

%type <node> program declaration instruction private_declaration function_arg
%type <sequence> exprs instructions declarations private_declarations function_args
%type <expression> expr function_def function_call
%type <type> type non_void_type function_type
%type <lvalue> lval
%type <block> declarations_instructions block
%type <i> qualifier
%type <t> types

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%


file : declarations            { compiler->ast(new cdk::sequence_node(LINE, $1)); } 
     | declarations program    { compiler->ast(new cdk::sequence_node(LINE, $2, $1)); } /* TODO: FIXME! */
     | program                 { compiler->ast(new cdk::sequence_node(LINE, $1)); }
     | /* empty. */            { compiler->ast(new cdk::sequence_node(LINE)); } 
     ; 

declarations : declarations declaration { $$ = new cdk::sequence_node(LINE, $2, $1); }
             | declaration { $$ = new cdk::sequence_node(LINE, $1); }
             ;

private_declarations : private_declarations private_declaration { $$ = new cdk::sequence_node(LINE, $2, $1); }
                     | private_declaration { $$ = new cdk::sequence_node(LINE, $1); }
                     ;

/*? Maybe needs * in identifiers $$ */
declaration  : '(' qualifier type tIDENTIFIER ')'      { $$ = new til::var_declaration_node(LINE, $2, *$4, nullptr ,$3); }
             | '(' qualifier type tIDENTIFIER expr ')' { $$ = new til::var_declaration_node(LINE, $2, *$4, $5 , $3); }
             | '(' qualifier tVAR tIDENTIFIER expr ')' { $$ = new til::var_declaration_node(LINE, $2 , *$4, $5, nullptr); }
             | '(' qualifier tIDENTIFIER expr ')'      { $$ = new til::var_declaration_node(LINE, $2 , *$3, $4, nullptr); }
             | private_declaration { $$ = $1; }
             ;

private_declaration : '(' type tIDENTIFIER ')'      { $$ = new til::var_declaration_node(LINE, tPRIVATE, *$3, nullptr, $2); }
                    | '(' type tIDENTIFIER expr ')' { $$ = new til::var_declaration_node(LINE, tPRIVATE, *$3, $4, $2); }
                    | '(' tVAR tIDENTIFIER expr ')' { $$ = new til::var_declaration_node(LINE, tPRIVATE ,*$3 , $4, nullptr); }

program : '(' tPROGRAM declarations_instructions ')' { $$ = new til::program_node(LINE, $3); }
        ;

declarations_instructions: private_declarations instructions   { $$ = new til::block_node(LINE, $1, $2); }
                         | private_declarations                { $$ = new til::block_node(LINE, $1 , new cdk::sequence_node(LINE)); }
                         | instructions                        { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), $1); }
                         |                                     { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
                         ;

type : non_void_type  { $$ = $1; }
     | tVOID          { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
     ;

non_void_type : tINT    { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
              | tDOUB   { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
              | tSTR    { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
              | type '!' { $$ = cdk::reference_type::create(4, $1); }
              | function_type { $$ = $1; }
              ;

function_type  :  '(' type ')'               { $$ = cdk::functional_type::create(std::vector<std::shared_ptr<cdk::basic_type>>(), $2); }
               |  '(' type '(' types ')' ')' { $$ = cdk::functional_type::create(*$4, $2); }
               ;

//TODO: check/ask when to use new keyword or not.

//TODO: legal? argumentos de funcoes teem certas restricoes, tipo nao poderem usar var.
// Nao tenho 100% certeza de estarmos a cumprir esse criterios.
function_def : '(' tFUNCTION '(' type function_args ')' declarations_instructions ')'   { $$ = new til::function_node(LINE, $5, $7, $4); }
             | '(' tFUNCTION '(' type ')' declarations_instructions ')'                 { $$ = new til::function_node(LINE, new cdk::sequence_node(LINE), $6, $4); }
             ; 
          
function_call : '(' expr exprs ')'      { $$ = new til::call_node(LINE, $2, $3); }
              | '(' expr ')'                           { $$ = new til::call_node(LINE, $2, new cdk::sequence_node(LINE)); }
              | '(' tRECURSION ')'                     { $$ = new til::call_node(LINE, nullptr, new cdk::sequence_node(LINE)); }
              | '(' tRECURSION exprs ')'{ $$ = new til::call_node(LINE, nullptr,$3); }
              ;

function_args : function_args function_arg { $$ = new cdk::sequence_node(LINE, $2, $1); }
              | function_arg { $$ = new cdk::sequence_node(LINE, $1); }
              ;

function_arg  : '(' type tIDENTIFIER ')' { $$ = new til::var_declaration_node(LINE, tPRIVATE, *$3, nullptr, $2); }
              ; 

types : type { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1);}
      | types type { $1->push_back($2); $$ = $1; } /* TODO: types type ou type types? i.e,recursao a esquerda ou a direita */
      ;

block : '(' tBLOCK declarations_instructions ')' { $$ = $3; }
      ;

qualifier : tPUBLIC      { $$ = tPUBLIC; }
          | tEXTERNAL    { $$ = tEXTERNAL; }
          | tFORWARD     { $$ = tFORWARD; }
          ;

instructions: instructions instruction  { $$ = new cdk::sequence_node(LINE, $2, $1); }
            | instruction               { $$ = new cdk::sequence_node(LINE, $1); }
            ;

instruction : expr                                     {$$ = new til::evaluation_node(LINE, $1); }
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
            ;

exprs : expr                      { $$ = new cdk::sequence_node(LINE, $1); }
      | exprs expr                { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

expr : tINTEGER                      { $$ = new cdk::integer_node(LINE, $1); }
     | tSTRING                       { $$ = new cdk::string_node(LINE, $1); }
     | tDOUBLE                       { $$ = new cdk::double_node(LINE, $1); }
     | tNULL                         { $$ = new til::nullptr_node(LINE); }
     | '(' tREAD ')'                 { $$ = new til::read_node(LINE); }
     | '(' '-' expr %prec tUNARY ')' { $$ = new cdk::unary_minus_node(LINE, $3); }
     | '(' '+' expr %prec tUNARY ')' { $$ = new cdk::unary_plus_node(LINE, $3); }
     | '(' '+' expr expr ')'         { $$ = new cdk::add_node(LINE, $3, $4); }
     | '(' '-' expr expr ')'         { $$ = new cdk::sub_node(LINE, $3, $4); }
     | '(' '*' expr expr ')'         { $$ = new cdk::mul_node(LINE, $3, $4); }
     | '(' '/' expr expr ')'         { $$ = new cdk::div_node(LINE, $3, $4); }
     | '(' '%' expr expr ')'         { $$ = new cdk::mod_node(LINE, $3, $4); }
     | '(' '<' expr expr ')'         { $$ = new cdk::lt_node(LINE, $3, $4); }
     | '(' '>' expr expr ')'         { $$ = new cdk::gt_node(LINE, $3, $4); }
     | '(' tGE expr expr ')'         { $$ = new cdk::ge_node(LINE, $3, $4); }
     | '(' tLE expr expr ')'         { $$ = new cdk::le_node(LINE, $3, $4); }
     | '(' tNE expr expr ')'         { $$ = new cdk::ne_node(LINE, $3, $4); }
     | '(' tEQ expr expr ')'         { $$ = new cdk::eq_node(LINE, $3, $4); }
     | '(' '~' expr %prec tUNARY ')' { $$ = new cdk::not_node(LINE, $3); }
     | '(' tAND expr expr ')'        { $$ = new cdk::and_node(LINE, $3, $4); }
     | '(' tOR expr expr ')'         { $$ = new cdk::or_node(LINE, $3, $4); }
     | lval                          { $$ = new cdk::rvalue_node(LINE, $1); }
     | '(' tSET lval expr ')'        { $$ = new cdk::assignment_node(LINE, $3, $4); }
     | '(' tSIZEOF expr ')'          { $$ = new til::sizeof_node(LINE, $3); }
     | '(' tOBJECTS expr ')'         { $$ = new til::alloc_node(LINE, $3); }
     | function_def                  { $$ = $1;}
     | function_call                 { $$ = $1; }

          //TODO: parse all call_node (i.e, function call & self-call) possibilities

     ;

lval : tIDENTIFIER                 { $$ = new cdk::variable_node(LINE, $1); }
     | '(' tINDEX expr expr ')'    { $$ = new til::index_node(LINE, $3, $4); }
     ;

%%
