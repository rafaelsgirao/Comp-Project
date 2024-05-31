#ifndef __TIL_TARGETS_POSTFIX_WRITER_H__
#define __TIL_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <cdk/emitters/basic_postfix_emitter.h>
#include <cdk/types/functional_type.h>
#include <optional>
#include <queue>
#include <set>
#include <sstream>
#include <unordered_set>
#include <vector>

namespace til {

class whileConds {
public:
  int next;
  int stop;
  whileConds(int next, int stop) : next(next), stop(stop) {}
};

class functionQueue {
public:
  int label;
  til::function_node *node;
  functionQueue(int next, til::function_node *node) : label(next), node(node) {}
};

//!
//! Traverse syntax tree and generate the corresponding assembly code.
//!
class postfix_writer : public basic_ast_visitor {
  cdk::symbol_table<til::symbol> &_symtab;
  std::unordered_set<std::string> _externSymbols;
  std::optional<std::string>
      _externalFunctionName; // name of external function to be called, if any
  std::queue<functionQueue> _functionQueue;
  std::set<std::string>
      _externalFunctionsToDeclare;     // set of external functions to declare
  std::vector<whileConds> _whileConds; // (next, stop)
  std::shared_ptr<cdk::functional_type> _functionType;
  cdk::basic_postfix_emitter &_pf;
  int _function;
  int _lbl;
  long _offset;

public:
  postfix_writer(std::shared_ptr<cdk::compiler> compiler,
                 cdk::symbol_table<til::symbol> &symtab,
                 cdk::basic_postfix_emitter &pf)
      : basic_ast_visitor(compiler), _symtab(symtab), _functionType(nullptr),
        _pf(pf), _lbl(0), _offset(0) {}

public:
  ~postfix_writer() { os().flush(); }

private:
  void cast(std::shared_ptr<cdk::basic_type> from,
            std::shared_ptr<cdk::basic_type> to);
  void handleCast(cdk::expression_node *const source_node,
                                     std::shared_ptr<cdk::basic_type> target_type,
                                     int level);
  void processCmpExpression(cdk::binary_operation_node *const node, int lvl);

  /** Method used to generate sequential labels. */
  inline std::string mklbl(int lbl) {
    std::ostringstream oss;
    if (lbl < 0)
      oss << ".L" << -lbl;
    else
      oss << "_L" << lbl;
    return oss.str();
  }

public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end
};

} // namespace til

#endif
