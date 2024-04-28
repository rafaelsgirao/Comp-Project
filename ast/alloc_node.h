#ifndef __TIL_AST_ALLOC_NODE_H__
#define __TIL_AST_ALLOC_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing alloc nodes.
   */
  class alloc_node : public cdk::expression_node {
    cdk::expression_node *_arg;


  public:
    alloc_node(int lineno, cdk::expression_node *arg) :
        cdk::expression_node(lineno) , _arg(arg) {
    }


    void accept(basic_ast_visitor *sp, int level) { sp->do_alloc_node(this, level); }

  };

} // til

#endif
