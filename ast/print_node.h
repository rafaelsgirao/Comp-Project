#ifndef __TIL_AST_PRINT_NODE_H__
#define __TIL_AST_PRINT_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing print nodes.
   */
  class print_node : public cdk::basic_node {
    cdk::expression_node *_argument;
    bool _newline;

  public:
    print_node(int lineno, cdk::expression_node *argument, bool newline) :
        cdk::basic_node(lineno), _argument(argument), _newline(newline) {
    }

    inline bool newline() {
      return _newline;
    }

    cdk::expression_node *argument() { return _argument; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_print_node(this, level); }

  };

} // til

#endif
