#ifndef __SIMPLE_AST_STOP_NODE_H__
#define __SIMPLE_AST_STOP_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing if-then nodes.
   */
  class stop_node : public cdk::basic_node {
    int _nth_cycle;

  public:
    stop_node(int lineno, int nth_cycle) :
        cdk::basic_node(lineno), _nth_cycle(nth_cycle) {
          //empty
    }

    stop_node(int lineno): cdk::basic_node(lineno), _nth_cycle(1) {
      //empty
    }

    void accept(basic_ast_visitor *sp, int level) { sp->do_stop_node(this, level); }

  };

} // til

#endif
