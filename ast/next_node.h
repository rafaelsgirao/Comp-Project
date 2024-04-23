#ifndef __SIMPLE_AST_NEXT_NODE_H__
#define __SIMPLE_AST_NEXT_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing if-then nodes.
   */
  class next_node : public cdk::basic_node {
    cdk::integer_node _nth_cycle;

  public:
    next_node(int lineno, cdk::integer_node n) :
        cdk::basic_node(lineno), _nth_cycle(n) {
          //Empty
    }

    next_node(int lineno) : _nth_cycle(cdk::integer_node(lineno, 1)) {
      //Empty

    }

    void accept(basic_ast_visitor *sp, int level) { sp->do_next_node(this, level); }

  };

} // til

#endif
