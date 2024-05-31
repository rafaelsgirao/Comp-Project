#ifndef __TIL_FACTORY_H__
#define __TIL_FACTORY_H__

#include "til_scanner.h"
#include <cdk/yy_factory.h>
#include <memory>

namespace til {

/**
 * This class implements the compiler factory for the TIL compiler.
 */
class factory : public cdk::yy_factory<til_scanner> {
  /**
   * This object is automatically registered by the constructor in the
   * superclass' language registry.
   */
  static factory _self;

protected:
  /**
   * @param language name of the language handled by this factory (see .cpp
   * file)
   */
  factory(const std::string &language = "til")
      : cdk::yy_factory<til_scanner>(language) {}
};

} // namespace til

#endif
