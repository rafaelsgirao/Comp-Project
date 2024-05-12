#ifndef __TIL_AST_CALL_NODE_H__
#define __TIL_AST_CALL_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing call nodes.
   */
  class call_node : public cdk::expression_node {

		cdk::expression_node *_function;
		cdk::sequence_node *_args;

  public:
    call_node(int lineno, cdk::expression_node *function, cdk::sequence_node *args) :
        cdk::expression_node(lineno), _function(function), _args(args) {
    }

    cdk::expression_node *function() {
      return _function;
    }

    cdk::sequence_node *args() {
      return _args;
    }

    void accept(basic_ast_visitor *sp, int level) { sp->do_call_node(this, level); }

  };

} // til

#endif
