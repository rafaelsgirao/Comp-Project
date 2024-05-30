#include "targets/postfix_writer.h"
#include ".auto/all_nodes.h" // all_nodes.h is automatically generated
#include "targets/frame_size_calculator.h"
#include "targets/type_checker.h"
#include "til_parser.tab.h"
#include <sstream>
#include <string>

void til::postfix_writer::cast(std::shared_ptr<cdk::basic_type> from,
                               std::shared_ptr<cdk::basic_type> to) {
  if (to->name() == cdk::TYPE_DOUBLE && from->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (to->name() == cdk::TYPE_POINTER && from->name() == cdk::TYPE_INT) {
    auto referenced = cdk::reference_type::cast(to)->referenced();
    _pf.INT(referenced->size() == 0 ? 1 : referenced->size());
    _pf.MUL();
  }
}

void til::postfix_writer::visitCast(cdk::expression_node *const from,
                                    std::shared_ptr<cdk::basic_type> to,
                                    int lvl) {
  if (!from->is_typed(cdk::TYPE_FUNCTIONAL)) {
    if (_functionType == nullptr && to->name() == cdk::TYPE_DOUBLE &&
        from->is_typed(cdk::TYPE_INT)) {
      auto integer = static_cast<cdk::literal_node<int> *>(from);
      _pf.SDOUBLE(static_cast<double>(integer->value()));
      return;
    }

    from->accept(this, lvl);
    cast(from->type(), to);
    return;
  }

  auto fromFunc = cdk::functional_type::cast(from->type());
  auto toFunc = cdk::functional_type::cast(to);
  if (type_cmp(fromFunc, toFunc)) {
    from->accept(this, lvl);
    return;
  }

  int lbl1 = ++_lbl, lbl2 = 0;

  if (_functionType != nullptr) {
    _pf.ADDR(mklbl(lbl1));
    _pf.JMP(mklbl(lbl2 = ++_lbl));
  } else {
    _pf.SADDR(mklbl(lbl1));
    _pf.TEXT();
  }

  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1));
  _pf.ENTER(0);

  int offset = 8 + static_cast<int>(fromFunc->input()->size());
  for (std::size_t i = toFunc->input_length(); i > 0; --i) {
    offset -= fromFunc->input(i - 1)->size();
    _pf.LOCAL(offset);
    if (toFunc->input(i - 1)->name() == cdk::TYPE_DOUBLE) {
      _pf.LDDOUBLE();
    } else {
      _pf.LDINT();
    }
    cast(toFunc->input(i - 1), fromFunc->input(i - 1));
  }

  from->accept(this, lvl);
  _pf.BRANCH();
  if (fromFunc->input_length() > 0) {
    _pf.TRASH(static_cast<int>(fromFunc->input()->size()));
  }

  if (fromFunc->output(0)->name() == cdk::TYPE_DOUBLE) {
    _pf.LDFVAL64();
  } else if (fromFunc->output(0)->name() != cdk::TYPE_VOID) {
    _pf.LDFVAL32();
  }

  cast(fromFunc->output(0), toFunc->output(0));

  if (toFunc->output(0)->name() == cdk::TYPE_DOUBLE) {
    _pf.STFVAL64();
  } else if (toFunc->output(0)->name() != cdk::TYPE_VOID) {
    _pf.STFVAL32();
  }

  _pf.LEAVE();
  _pf.RET();

  if (_functionType != nullptr) {
    _pf.LABEL(mklbl(lbl2));
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void til::postfix_writer::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_sequence_node(cdk::sequence_node *const node,
                                           int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_integer_node(cdk::integer_node *const node,
                                          int lvl) {
  if (_functionType == nullptr) {
    _pf.SINT(node->value());
  } else {
    _pf.INT(node->value()); // push an integer
  }
}

void til::postfix_writer::do_double_node(cdk::double_node *const node,
                                         int lvl) {
  if (_functionType == nullptr) {
    _pf.SDOUBLE(node->value());
  } else {
    _pf.DOUBLE(node->value()); // push a double
  }
}

void til::postfix_writer::do_string_node(cdk::string_node *const node,
                                         int lvl) {
  int lbl1 = ++_lbl;

  /* generate the string */
  _pf.RODATA();               // strings are DATA readonly
  _pf.ALIGN();                // make sure we are aligned
  _pf.LABEL(mklbl(lbl1));     // give the string a name
  _pf.SSTRING(node->value()); // output string characters

  if (_functionType == nullptr) {
    _pf.DATA();
    _pf.SADDR(mklbl(lbl1));
  } else {
    /* leave the address on the stack */
    _pf.TEXT();            // return to the TEXT segment
    _pf.ADDR(mklbl(lbl1)); // the string to be printed
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_add_node(cdk::add_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  visitCast(node->left(), node->type(), lvl);
  visitCast(node->right(), node->type(), lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DADD();
  } else {
    _pf.ADD();
  }
}
void til::postfix_writer::do_sub_node(cdk::sub_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  visitCast(node->left(), node->type(), lvl);
  visitCast(node->right(), node->type(), lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DSUB();
  } else {
    _pf.SUB();
  }

  if (node->is_typed(cdk::TYPE_INT) &&
      node->left()->is_typed(cdk::TYPE_POINTER)) {
    auto referenced =
        cdk::reference_type::cast(node->left()->type())->referenced();
    _pf.INT(referenced->size() == 0 ? 1 : referenced->size());
    _pf.DIV();
  }
}
void til::postfix_writer::do_mul_node(cdk::mul_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  visitCast(node->left(), node->type(), lvl);
  visitCast(node->right(), node->type(), lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DMUL();
  } else {
    _pf.MUL();
  }
}
void til::postfix_writer::do_div_node(cdk::div_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  visitCast(node->left(), node->type(), lvl);
  visitCast(node->right(), node->type(), lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DDIV();
  } else {
    _pf.DIV();
  }
}
void til::postfix_writer::do_mod_node(cdk::mod_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}

void til::postfix_writer::processCmpExpression(
    cdk::binary_operation_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_INT) &&
      node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) &&
      node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) ||
      node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
}

void til::postfix_writer::do_unary_minus_node(cdk::unary_minus_node *const node,
                                              int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG();                           // 2-complement
}

void til::postfix_writer::do_unary_plus_node(cdk::unary_plus_node *const node,
                                             int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
}

void til::postfix_writer::do_lt_node(cdk::lt_node *const node, int lvl) {
  processCmpExpression(node, lvl);
  _pf.LT();
}
void til::postfix_writer::do_le_node(cdk::le_node *const node, int lvl) {
  processCmpExpression(node, lvl);
  _pf.LE();
}
void til::postfix_writer::do_ge_node(cdk::ge_node *const node, int lvl) {
  processCmpExpression(node, lvl);
  _pf.GE();
}
void til::postfix_writer::do_gt_node(cdk::gt_node *const node, int lvl) {
  processCmpExpression(node, lvl);
  _pf.GT();
}
void til::postfix_writer::do_ne_node(cdk::ne_node *const node, int lvl) {
  processCmpExpression(node, lvl);
  _pf.NE();
}
void til::postfix_writer::do_eq_node(cdk::eq_node *const node, int lvl) {
  processCmpExpression(node, lvl);
  _pf.EQ();
}

void til::postfix_writer::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.INT(0);
  _pf.EQ();
}

void til::postfix_writer::do_and_node(cdk::and_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1 = ++_lbl;
  node->left()->accept(this, lvl);
  _pf.DUP32();
  _pf.JZ(mklbl(lbl1));
  _pf.TRASH(4);
  node->right()->accept(this, lvl);
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1));
}

void til::postfix_writer::do_or_node(cdk::or_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1 = ++_lbl;
  node->left()->accept(this, lvl);
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl1));
  _pf.TRASH(4);
  node->right()->accept(this, lvl);
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_variable_node(cdk::variable_node *const node,
                                           int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto symbol = _symtab.find(node->name());
  if (symbol->node()->qualifier() == tEXTERNAL) {
    _pf.ADDR("_EXTERNAL_" + node->name());
  } else if (symbol->offset() == 1) {
    _pf.ADDR(node->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void til::postfix_writer::do_rvalue_node(cdk::rvalue_node *const node,
                                         int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  if (node->lvalue()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDDOUBLE();
  } else {
    _pf.LDINT();
  }

  cast(node->lvalue()->type(), node->type());
}

void til::postfix_writer::do_assignment_node(cdk::assignment_node *const node,
                                             int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  visitCast(node->rvalue(), node->lvalue()->type(), lvl);
  if (node->lvalue()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl);
  if (node->lvalue()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.STDOUBLE();
  } else {
    _pf.STINT();
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_block_node(til::block_node *const node, int lvl) {
  node->declarations()->accept(this, lvl);
  node->instructions()->accept(this, lvl);
}

void til::postfix_writer::do_return_node(til::return_node *const node,
                                         int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (node->returnval() != nullptr) {
    auto returnType = cdk::functional_type::cast(_functionType)->output(0);

    visitCast(node->returnval(), returnType, lvl);
    if (returnType->name() == cdk::TYPE_DOUBLE) {
      _pf.STFVAL64();
    } else {
      _pf.STFVAL32();
    }
  }

  _pf.LEAVE();
  _pf.RET();
}

void til::postfix_writer::do_stop_node(til::stop_node *const node, int lvl) {
  if (static_cast<std::size_t>(node->nth_cycle()) > _loopLabels.size() ||
      node->nth_cycle() < 1) {
    std::cerr << (node)->lineno()
              << ": invalid nesting level for stop instruction" << std::endl;
    return;
  }

  auto lbl = _loopLabels[_loopLabels.size() - node->nth_cycle()].second;
  _pf.JMP(mklbl(lbl));
}

void til::postfix_writer::do_next_node(til::next_node *const node, int lvl) {
  if (static_cast<std::size_t>(node->nth_cycle()) > _loopLabels.size() ||
      node->nth_cycle() < 1) {
    std::cerr << (node)->lineno()
              << ": invalid nesting level for next instruction" << std::endl;
  }

  auto lbl = _loopLabels[_loopLabels.size() - node->nth_cycle()].first;
  _pf.JMP(mklbl(lbl));
}

void til::postfix_writer::do_nullptr_node(til::nullptr_node *const node,
                                          int lvl) {
  if (_functionType == nullptr) {
    _pf.SINT(0);
  } else {
    _pf.INT(0);
  }
}

void til::postfix_writer::do_sizeof_node(til::sizeof_node *const node,
                                         int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.TRASH(node->argument()->type()->size());
  _pf.INT(node->argument()->type()->size());
}

void til::postfix_writer::do_var_declaration_node(
    til::var_declaration_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto symbol = _symtab.find(node->name());

  if (node->qualifier() == tEXTERNAL) {
    _pf.RODATA();
    _pf.ALIGN();
    _pf.LABEL("_EXTERNAL_" + node->name());
    _pf.EXTERN(node->name());
    _pf.SADDR(node->name());
    return;
  }

  if (node->qualifier() == tFORWARD) {
    if (symbol->node() == node) {
      _externalFunctionsToDeclare.insert(node->name());

      // Last declaration is this forward, must mark symbol as external.
      //    _pf.EXTERN(node->name());
    }
    return;
  }

  if (_functionType == nullptr) {
    if (node->init() == nullptr) {
      _pf.BSS();
    } else {
      _pf.DATA();
    }
    _pf.ALIGN();

    if (node->qualifier() == tPUBLIC) {
      _pf.GLOBAL(node->name(), _pf.OBJ());
    }

    _pf.LABEL(node->name());
    if (node->init() == nullptr) {
      _pf.SALLOC(node->type()->size());
    } else {
      visitCast(node->init(), node->type(), lvl);
    }
  } else {
    _offset -= node->type()->size();
    symbol->offset(_offset);

    if (node->init() != nullptr) {
      visitCast(node->init(), node->type(), lvl);
      _pf.LOCAL(_offset);
      if (node->type()->name() == cdk::TYPE_DOUBLE) {
        _pf.STDOUBLE();
      } else {
        _pf.STINT();
      }
    }
  }
}

void til::postfix_writer::do_call_node(til::call_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto functionType =
      node->function() == nullptr
          ? _functionType
          : cdk::functional_type::cast(node->function()->type());

  // Push arguments in reverse order.
  long argsSize = 0;
  for (auto i = node->args()->size(); i > 0; --i) {
    auto exp = static_cast<cdk::expression_node *>(node->args()->node(i - 1));
    visitCast(exp, functionType->input(i - 1), lvl);
    argsSize += functionType->input(i - 1)->size();
  }

  if (node->function()) {
    node->function()->accept(this, lvl);
    _pf.BRANCH();
  } else {
    _pf.CALL(mklbl(_function));
  }

  // Clean up arguments before pushing the output.
  if (argsSize > 0) {
    _pf.TRASH(argsSize);
  }

  if (functionType->output(0)->name() == cdk::TYPE_DOUBLE) {
    _pf.LDFVAL64();
  } else if (functionType->output(0)->name() != cdk::TYPE_VOID) {
    _pf.LDFVAL32();
  }
}

void til::postfix_writer::do_index_node(til::index_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->base()->accept(this, lvl);
  node->ind()->accept(this, lvl);

  auto referenced =
      cdk::reference_type::cast(node->base()->type())->referenced();
  _pf.INT(referenced->size() == 0 ? 1 : referenced->size());
  _pf.MUL();
  _pf.ADD();
}

void til::postfix_writer::do_address_of_node(til::address_of_node *const node,
                                             int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
}

void til::postfix_writer::do_alloc_node(til::alloc_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.INT(cdk::reference_type::cast(node->type())->referenced()->size());
  _pf.MUL();
  _pf.ALLOC();
  _pf.SP();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_function_node(til::function_node *const node,
                                           int lvl) {
  int lbl = 0;

  // GIRAO FIXME: NOS TEMOS PROGRAM_NODE E ELES NAO
  if (_functionType != nullptr) {
    // Nested function! Defer its definition to the end of the parent function.
    _pf.ADDR(mklbl(lbl = ++_lbl));
    _deferredFunctions.push({lbl, node});
    return;
  }

  if (!_deferredFunctions.empty()) {
    // We are defining a previously deferred function - get its label.
    lbl = _deferredFunctions.front().first;
    _deferredFunctions.pop();
  }
  // This is a function expression on a global variable.
  _pf.SADDR(mklbl(lbl = ++_lbl));

  ASSERT_SAFE_EXPRESSIONS;

  // Get the size of the function frame.
  til::frame_size_calculator fsc(_compiler, _symtab, _functionType);
  node->block()->accept(&fsc, lvl);

  _functionType = cdk::functional_type::cast(node->type());
  _offset = 0;
  _function = lbl;

  _pf.TEXT();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
  _pf.ENTER(fsc.size());

  _symtab.push();

  long argOffset = 8;
  for (std::size_t i = 0; i < node->args()->size(); ++i) {
    auto decl = static_cast<til::var_declaration_node *>(node->args()->node(i));
    _symtab.insert(decl->name(),
                   std::make_shared<til::symbol>(decl, argOffset));
    argOffset += decl->type()->size();
  }

  node->block()->accept(this, lvl);
  _symtab.pop();

  _pf.LEAVE();
  _pf.RET();

  _functionType = nullptr;

  if (!_deferredFunctions.empty()) {
    // We have deferred functions! Let's define them now.
    auto [lbl, function] = _deferredFunctions.front();
    function->accept(this, lvl);
  }
}

void til::postfix_writer::do_program_node(til::program_node *const node,
                                          int lvl) {

  _functionType = cdk::functional_type::create(
      cdk::primitive_type::create(4, cdk::TYPE_INT));

  // generate the main function (RTS mandates that its name be "_main")
  _pf.TEXT();
  _pf.ALIGN();
  _pf.GLOBAL("_main", _pf.FUNC());
  _pf.LABEL("_main");

  // Get the size of the function frame.
  til::frame_size_calculator fsc(_compiler, _symtab, _functionType);
  node->block()->accept(&fsc, lvl);

  _offset = 0;

  _pf.TEXT();
  _pf.ALIGN();
  _pf.ENTER(fsc.size());
  _symtab.push();
  // TODO: symtab push
  //
  node->block()->accept(this, lvl);
  _symtab.pop();

  // TODO: o codigo abaixo deveria vir do return node
  //  end the main function
  _pf.INT(0);
  _pf.STFVAL32();
  _pf.LEAVE();
  _pf.RET();

  // functionLabels.pop
  // symtab..pop
  // TODO: do this automatically with a for loop
  // these are just a few library function imports
  _pf.EXTERN("readi");
  _pf.EXTERN("printi");
  _pf.EXTERN("prints");
  _pf.EXTERN("println");
  for (auto name : _externalFunctionsToDeclare) {
    _pf.EXTERN(name);
  }
  return;
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_evaluation_node(til::evaluation_node *const node,
                                             int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);         // determine the value
  _pf.TRASH(node->argument()->type()->size()); // delete it
}

void til::postfix_writer::do_print_node(til::print_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  for (auto *argument : node->arguments()->nodes()) {
    auto *typed = static_cast<cdk::typed_node *>(argument);
    typed->accept(this, lvl); // determine the value to print
    if (typed->is_typed(cdk::TYPE_INT)) {
      _externalFunctionsToDeclare.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4); // delete the printed value
    } else if (typed->is_typed(cdk::TYPE_DOUBLE)) {
      _externalFunctionsToDeclare.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8); // delete the printed value
    } else if (typed->is_typed(cdk::TYPE_STRING)) {
      _externalFunctionsToDeclare.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4); // delete the printed value's address
    }
  }

  if (node->newline()) {
    _externalFunctionsToDeclare.insert("println");
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_read_node(til::read_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (node->is_typed(cdk::TYPE_INT)) {
    _externalFunctionsToDeclare.insert("readi");

    _pf.CALL("readi");
    _pf.LDFVAL32();
  } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _externalFunctionsToDeclare.insert("readd");

    _pf.CALL("readd");
    _pf.LDFVAL64();
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_loop_node(til::loop_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  _pf.LABEL(mklbl(lbl1 = ++_lbl));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl2 = ++_lbl));
  _loopLabels.push_back({lbl1, lbl2});
  node->block()->accept(this, lvl + 2);
  _loopLabels.pop_back();
  _pf.JMP(mklbl(lbl1));
  _pf.LABEL(mklbl(lbl2));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_node(til::if_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_else_node(til::if_else_node *const node,
                                          int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1 = lbl2));
}