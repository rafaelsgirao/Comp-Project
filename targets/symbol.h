#ifndef __TIL_TARGETS_SYMBOL_H__
#define __TIL_TARGETS_SYMBOL_H__

#include <cdk/types/basic_type.h>
#include <memory>
#include <string>

namespace til {
class var_declaration_node;

class symbol {
  var_declaration_node *_node;
  /** when offset is 1, symbol is global */
  long _offset;

public:
  symbol(var_declaration_node *node, long offset = 1)
      : _node(node), _offset(offset) {}

  virtual ~symbol() {
    // EMPTY
  }

public:
  void node(var_declaration_node *node) { _node = node; }
  var_declaration_node *node() const { return _node; }
  void offset(long value) { _offset = value; }
  long offset() const { return _offset; }
};

} // namespace til

#endif
