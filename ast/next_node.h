#ifndef __SIMPLE_AST_NEXT_NODE_H__
#define __SIMPLE_AST_NEXT_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing if-then nodes.
   */
  class next_node : public cdk::basic_node {
    int _nth_cycle;

  public:
    next_node(int lineno, int n) :
        cdk::basic_node(lineno), _nth_cycle(n) {
    }

    next_node(int lineno) : cdk::basic_node(lineno),  _nth_cycle(1) {
    }

    void accept(basic_ast_visitor *sp, int level) { sp->do_next_node(this, level); }
  };

} // til

#endif
