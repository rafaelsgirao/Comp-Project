#ifndef __TIL_AST_SIZEOF_NODE_H__
#define __TIL_AST_SIZEOF_NODE_H__

#include <cdk/ast/unary_operation_node.h>

namespace til {

  /**
   * Class for describing sizeof nodes.
   */
  class sizeof_node : public cdk::unary_operation_node {

  public:
    sizeof_node(int lineno, cdk::expression_node *expression) :
        cdk::unary_operation_node(lineno, expression) {
    }

    void accept(basic_ast_visitor *sp, int level) { sp->do_sizeof_node(this, level); }

  };

} // til

#endif
