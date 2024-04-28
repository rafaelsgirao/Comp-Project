#ifndef __TIL_AST_FUNCTION_NODE_H__
#define __TIL_AST_FUNCTION_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing function nodes.
   */
  class function_node : public cdk::expression_node {
		

  public:
    function_node(int lineno, int nth_cycle) :
        cdk::expression_node(lineno), _nth_cycle(nth_cycle) {
          //empty
    }


    void accept(basic_ast_visitor *sp, int level) { sp->do_function_node(this, level); }

  };

} // til

#endif
