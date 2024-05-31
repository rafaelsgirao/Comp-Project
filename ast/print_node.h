#ifndef __TIL_AST_PRINT_NODE_H__
#define __TIL_AST_PRINT_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

/**
 * Class for describing print nodes.
 */
class print_node : public cdk::basic_node {
  cdk::sequence_node *_arguments;
  bool _newline;

public:
  print_node(int lineno, cdk::sequence_node *arguments, bool newline)
      : cdk::basic_node(lineno), _arguments(arguments), _newline(newline) {}

  inline bool newline() { return _newline; }

  cdk::sequence_node *arguments() { return _arguments; }

  void accept(basic_ast_visitor *sp, int level) {
    sp->do_print_node(this, level);
  }
};

} // namespace til

#endif
