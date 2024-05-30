#ifndef __SIMPLE_TARGETS_SYMBOL_H__
#define __SIMPLE_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>


namespace til {

  class symbol {
    std::shared_ptr<cdk::basic_type> _type;
    std::string _name;
    long _value; // hack!    
    int _qualifier = 276;
    long _offset;

  public:
    symbol(std::shared_ptr<cdk::basic_type> type, const std::string &name, long value, long offset = 0) :
        _type(type), _name(name), _value(value), _offset(offset) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }
    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }
    const std::string &name() const {
      return _name;
    }
    long value() const {
      return _value;
    }
    long value(long v) {
      return _value = v;
    }

    int qualifier() const {
      return _qualifier;
    }

    void qualifier(int q) {
      _qualifier = q;
    }

    void offset(long o) {
      _offset = o;
    }

    long offset() const {
      return _offset;
    }


  };

  inline auto create_symbol(std::shared_ptr<cdk::basic_type> type, const std::string &name, long value, long offset = 0) {
    return std::make_shared<symbol>(type, name, value, offset);
  }

} // til

#endif
