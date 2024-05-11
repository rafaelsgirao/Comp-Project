#ifndef __TIL_AST_ADDRESSOF_NODE_H__
#define __TIL_AST_ADDRESSOF_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>

namespace til {

  /**
   * Class for describing nodes' addresses.
   */
  class address_of_node : public cdk::expression_node {
		cdk::lvalue_node *_lvalue;

  public:
    address_of_node(int lineno, cdk::lvalue_node *lval) :
        cdk::expression_node(lineno), _lvalue(lval) {
    }

    cdk::lvalue_node *lvalue() {
      return _lvalue;
    }

    void accept(basic_ast_visitor *sp, int level) { sp->do_address_of_node(this, level); }

  };

} // til

#endif
