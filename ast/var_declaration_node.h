#ifndef __SIMPLE_AST_VAR_NODE_H__
#define __SIMPLE_AST_VAR_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing var nodes.
   */
  class var_declaration_node : public cdk::typed_node {
		int _qualifier;
    std::string _name;
    cdk::expression_node *_init;

  public:
    var_declaration_node(int lineno, int qualifier, std::string &name, cdk::expression_node *init, shared_ptr<cdk::basic_type> type = nullptr) : 
        cdk::typed_node(lineno), _qualifier(qualifier) ,_name(name), _init(init) {
          type(type);
    }

    var_declaration_node(int lineno, int qualifier, std::string &name, shared_ptr<cdk::basic_type> type = nullptr) : 
        cdk::typed_node(lineno), _qualifier(qualifier) ,_name(name), _init(nullptr) {
          type(type);
    }

    void accept(basic_ast_visitor *sp, int level) { sp->do_var_declaration_node(this, level); }

  };

} // til

#endif
