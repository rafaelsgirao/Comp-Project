#ifndef __TIL_TARGETS_OFFSET_CHECKER_H__
#define __TIl_TARGETS_OFFSET_CHECKER_H__

#include "targets/basic_ast_visitor.h"
#include <cdk/types/functional_type.h>

namespace til {

/**
 * Finds the size of a function's frame.
 */
class offset_checker : public basic_ast_visitor {
  cdk::symbol_table<til::symbol> &_symtab;
  std::shared_ptr<cdk::functional_type> _functionType;
  std::size_t _size;

public:
  offset_checker(std::shared_ptr<cdk::compiler> compiler,
                 cdk::symbol_table<til::symbol> &symtab,
                 std::shared_ptr<cdk::functional_type> functionType)
      : basic_ast_visitor(compiler), _symtab(symtab),
        _functionType(functionType), _size(0) {}

public:
  std::size_t size() { return _size; }

public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end
};

} // namespace til

#endif
