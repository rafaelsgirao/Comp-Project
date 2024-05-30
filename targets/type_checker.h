#ifndef __TIL_TARGETS_TYPE_CHECKER_H__
#define __TIL_TARGETS_TYPE_CHECKER_H__

#include "targets/basic_ast_visitor.h"

#include <cdk/types/functional_type.h>

namespace til {
/**
 * Prints a type as a string.
 */
std::string to_string(std::shared_ptr<cdk::basic_type> type);

bool deep_type_cmp(std::shared_ptr<cdk::basic_type> lhs,
              std::shared_ptr<cdk::basic_type> rhs);

/**
 * Print nodes as XML elements to the output stream.
 */
class type_checker : public basic_ast_visitor {
  cdk::symbol_table<til::symbol> &_symtab;
  std::shared_ptr<cdk::functional_type> _functionType;

  bool _isPropagating = false;
  bool _isTesting = false;

public:
  type_checker(std::shared_ptr<cdk::compiler> compiler,
               cdk::symbol_table<til::symbol> &symtab,
               std::shared_ptr<cdk::functional_type> functionType)
      : basic_ast_visitor(compiler), _symtab(symtab),
        _functionType(functionType) {}

public:
  ~type_checker() { os().flush(); }

protected:
  void processUnaryExpression(cdk::unary_operation_node *const node, int lvl);
  void processMulExpression(cdk::binary_operation_node *const node, int lvl);
  void processCmpExpression(cdk::binary_operation_node *const node, int lvl);
  void processEqExpression(cdk::binary_operation_node *const node, int lvl);
  void processLogicalExpression(cdk::binary_operation_node *const node,
                                int lvl);
  template <typename T>
  void process_literal(cdk::literal_node<T> *const node, int lvl) {}

  std::shared_ptr<cdk::basic_type>
  unify_node_to_type(cdk::typed_node *const from,
                     std::shared_ptr<cdk::basic_type> to, int lvl);
  void unify_node_to_node(cdk::typed_node *const from,
                          cdk::typed_node *const to, int lvl);
  bool test_unify_node_to_type(cdk::typed_node *const from,
                               std::shared_ptr<cdk::basic_type> to, int lvl);
  void default_node_to_int(cdk::typed_node *const node, int lvl);
  void propagate(cdk::typed_node *const node, int lvl);

public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end
};

} // namespace til

//---------------------------------------------------------------------------
//     HELPER MACRO FOR TYPE CHECKING
//---------------------------------------------------------------------------

#define CHECK_TYPES(compiler, symtab, functionType, node)                      \
  {                                                                            \
    try {                                                                      \
      til::type_checker checker(compiler, symtab, functionType);               \
      (node)->accept(&checker, 0);                                             \
    } catch (const std::string &problem) {                                     \
      std::cerr << (node)->lineno() << ": " << problem << std::endl;           \
      return;                                                                  \
    }                                                                          \
  }

#define ASSERT_SAFE_EXPRESSIONS                                                \
  CHECK_TYPES(_compiler, _symtab, _functionType, node)

#endif
