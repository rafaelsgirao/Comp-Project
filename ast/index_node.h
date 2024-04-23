#ifndef __SIMPLE_AST_INDEX_NODE_H__
#define __SIMPLE_AST_INDEX_NODE_H__

#include <cdk/ast/lvalue_node.h>

namespace til {

  /**
   * Class for describing sizeof nodes.
   */
  class sizeof_node : public cdk::lvalue_node {
    cdk::expression_node *_base;
		cdk::expression_node *_ind;

  public:
    index_node(int lineno, cdk::expression_node *base, cdk::expression_node *ind) :
				cdk::lvalue_node(lineno), _base(base), _ind(ind) {
		}

    inline cdk::expression_node *base() { return _base; }

		inline cdk::expression_node *ind() { return _ind; }

		void accept(basic_ast_visitor *sp, int level) {
			sp->do_index_node(this, level);
		}

  };

} // til

#endif
