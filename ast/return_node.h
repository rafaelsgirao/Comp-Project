#ifndef __SIMPLE_AST_STOP_NODE_H__
#define __SIMPLE_AST_STOP_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing if-then nodes.
   */
  class return_node : public cdk::basic_node {
    cdk::expression_node *

  public:
    return_node(int lineno, cdk::expression_node *returnval) :
        cdk::basic_node(lineno), _returnval(returnval) {
          //Empty
    }

    void accept(basic_ast_visitor *sp, int level) { sp->do_return_node(this, level); }

  };

} // til

#endif
