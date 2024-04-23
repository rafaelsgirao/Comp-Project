#ifndef __SIMPLE_AST_NULLPTR_NODE_H__
#define __SIMPLE_AST_NULLPTR_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing if-then nodes.
   */
  class nullptr_node : public cdk::expression_node {

  public:
    nullptr_node(int lineno) :
        cdk::expression_node(lineno) {
          //Empty
    }

    void accept(basic_ast_visitor *sp, int level) { sp->do_nullptr_node(this, level); }

  };

} // til

#endif
