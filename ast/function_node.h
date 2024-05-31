#ifndef __TIL_AST_FUNCTION_NODE_H__
#define __TIL_AST_FUNCTION_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

/**
 * Class for describing function nodes.
 */
class function_node : public cdk::expression_node {
  cdk::sequence_node *_args;
  til::block_node *_block;

public:
  function_node(int lineno, cdk::sequence_node *args, til::block_node *block,
                std::shared_ptr<cdk::basic_type> return_type)
      : cdk::expression_node(lineno), _args(args), _block(block) {
    std::vector<std::shared_ptr<cdk::basic_type>> inputs;

    for (size_t i = 0; i < args->size(); i++) {
      inputs.push_back(dynamic_cast<cdk::typed_node *>(args->node(i))->type());
    }

    this->type(cdk::functional_type::create(inputs, return_type));
  }

  cdk::sequence_node *args() { return _args; }

  til::block_node *block() { return _block; }

  void accept(basic_ast_visitor *sp, int level) {
    sp->do_function_node(this, level);
  }
};

} // namespace til

#endif
