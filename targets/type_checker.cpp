#include "targets/type_checker.h"
#include ".auto/all_nodes.h" // automatically generated
#include <cdk/types/primitive_type.h>
#include <string>

#include "til_parser.tab.h"

#define ASSERT_UNSPEC                                                          \
  {                                                                            \
    if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC))          \
      return;                                                                  \
  }

//---------------------------------------------------------------------------

void til::type_checker::do_sequence_node(cdk::sequence_node *const node,
                                         int lvl) {
  for (auto child_node : node->nodes()) {
    child_node->accept(this, lvl + 2);
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}
void til::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processBinaryExpression(node, lvl, false, false);
}
void til::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processBinaryLogicalExpression(node, lvl, false, false);
}

//---------------------------------------------------------------------------

void til::type_checker::do_integer_node(cdk::integer_node *const node,
                                        int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void til::type_checker::processUnaryExpression(
    cdk::unary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  switch (node->argument()->type()->name()) {
  case cdk::TYPE_INT:
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    break;
  case cdk::TYPE_UNSPEC:
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    break;
  case cdk::TYPE_DOUBLE:
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    break;
  default:
    throw std::string("wrong type in unary expression");
  }
}

void til::type_checker::do_unary_minus_node(cdk::unary_minus_node *const node,
                                            int lvl) {
  processUnaryExpression(node, lvl);
}

void til::type_checker::do_unary_plus_node(cdk::unary_plus_node *const node,
                                           int lvl) {
  processUnaryExpression(node, lvl);
}

void til::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::processBinaryExpression(
    cdk::binary_operation_node *const node, int lvl, bool shouldCheckDouble,
    bool shouldCheckPointer) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);

  switch (node->left()->type()->name()) {
  case cdk::TYPE_INT:
  case cdk::TYPE_UNSPEC:
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_INT) ||
        (shouldCheckDouble && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
      node->type(node->right()->type());
    } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (shouldCheckPointer &&
               node->right()->is_typed(cdk::TYPE_POINTER)) {
      if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
        node->left()->type(node->right()->type());
      }

      node->type(node->right()->type());
    } else {
      throw std::string(
          "wrong type in right argument of arithmetic binary expression");
    }
    break;
  case cdk::TYPE_DOUBLE:
    if (!shouldCheckDouble)
      break; // This expression should not have a double type

    node->right()->accept(this, lvl + 2);

    if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_DOUBLE)) {
      node->type(node->right()->type());
    } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else {
      throw std::string(
          "wrong type in right argument of arithmetic binary expression");
    }
    break;
  case cdk::TYPE_POINTER:
    if (!shouldCheckPointer)
      break; // This expression should not have a pointer type
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_INT)) {
      node->type(node->left()->type());
    } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->type(node->left()->type());
    }
    // TODO CHECK BOTH POINTERS CASE

    else {
      throw std::string(
          "wrong type in right argument of arithmetic binary expression");
    }
    break;
  default:
    throw std::string(
        "wrong type in left argument of arithmetic binary expression");
  }
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::processBinaryLogicalExpression(
    cdk::binary_operation_node *const node, int lvl, bool shouldCheckDouble,
    bool shouldCheckPointer) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);

  switch (node->left()->type()->name()) {
  case cdk::TYPE_INT:
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(node->left()->type());
      break;
    } // todo check if you need to verify if its not any other type
    throw std::string(
        "wrong type in right argument of logical binary expression");

  case cdk::TYPE_DOUBLE:
    if (!shouldCheckDouble)
      break; // This expression should not have a double type

    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      break;
    } // todo check if you need to verify if its not any other type
    throw std::string(
        "wrong type in right argument of logical binary expression");

  case cdk::TYPE_POINTER:
    if (!shouldCheckPointer)
      break; // This expression should not have a pointer type
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(
          4, cdk::TYPE_INT)); // TODO Why is this int?
      break;
    } // todo check if you need to verify if its not any other type
    throw std::string(
        "wrong type in right argument of logical binary expression");

  case cdk::TYPE_UNSPEC: // TODO check if this is correct
    node->right()->accept(this, lvl + 2);
    if (node->right()->is_typed(cdk::TYPE_INT) ||
        (shouldCheckDouble && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
      node->type(node->right()->type());
    } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (shouldCheckPointer &&
               node->right()->is_typed(cdk::TYPE_POINTER)) {
      if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
        node->left()->type(node->right()->type());
      }

      node->type(node->right()->type());
    } else {
      throw std::string(
          "wrong type in right argument of arithmetic binary expression");
    }
    break;

  default:
    throw std::string(
        "wrong type in left argument of arithmetic binary expression");
  }
}

void til::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processBinaryExpression(node, lvl, true, true);
}
void til::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processBinaryExpression(node, lvl, true, true);
}
void til::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processBinaryExpression(node, lvl, true, false);
}
void til::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processBinaryExpression(node, lvl, true, false);
}
void til::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processBinaryExpression(node, lvl, true, false);
}
void til::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processBinaryLogicalExpression(node, lvl, true, false);
}
void til::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processBinaryLogicalExpression(node, lvl, true, false);
}
void til::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processBinaryLogicalExpression(node, lvl, true, false);
}
void til::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processBinaryLogicalExpression(node, lvl, true, false);
}
void til::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processBinaryLogicalExpression(node, lvl, true, true);
}
void til::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processBinaryLogicalExpression(node, lvl, true, true);
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node *const node,
                                         int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<til::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void til::type_checker::do_assignment_node(cdk::assignment_node *const node,
                                           int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 2);
  node->rvalue()->accept(this, lvl + 2);

  if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
    node->rvalue()->type(node->lvalue()->type());
  } else if (node->lvalue()->is_typed(node->rvalue()->type()->name()) &&
             node->lvalue()->is_typed(cdk::TYPE_POINTER)) {
    auto lvalref = cdk::reference_type::cast(node->lvalue()->type());
    auto rvalref = cdk::reference_type::cast(node->rvalue()->type());

    if (lvalref->referenced()->name() == cdk::TYPE_UNSPEC ||
        lvalref->referenced()->name() == cdk::TYPE_VOID ||
        rvalref->referenced()->name() == cdk::TYPE_VOID) {
      node->rvalue()->type(node->lvalue()->type());
    } else {
      throw std::string(
          "wrong type in right argument of assignment expression");
    }
  }

  // TODO: Check type compatibility

  node->type(node->lvalue()->type());
}

//---------------------------------------------------------------------------

void til::type_checker::do_program_node(til::program_node *const node,
                                        int lvl) {
  // EMPTY
}

void til::type_checker::do_evaluation_node(til::evaluation_node *const node,
                                           int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void til::type_checker::do_print_node(til::print_node *const node, int lvl) {

  for (size_t i = 0; i < node->arguments()->size(); i++) {
    auto child =
        dynamic_cast<cdk::expression_node *>(node->arguments()->node(i));

    child->accept(this, lvl);

    if (!(child->is_typed(cdk::TYPE_STRING) || child->is_typed(cdk::TYPE_INT) ||
          child->is_typed(cdk::TYPE_DOUBLE))) {
      throw std::string("wrong argument type for print instruction.");
    }
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_read_node(til::read_node *const node, int lvl) {
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void til::type_checker::do_loop_node(til::loop_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void til::type_checker::do_if_node(til::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);

  switch (node->condition()->type()->name()) {
  case cdk::TYPE_UNSPEC:
    node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  case cdk::TYPE_INT:
    break;
  default:
    throw std::string("wrong type in condition of if expression");
  }
}

void til::type_checker::do_if_else_node(til::if_else_node *const node,
                                        int lvl) {
  node->condition()->accept(this, lvl + 4);

  switch (node->condition()->type()->name()) {
  case cdk::TYPE_UNSPEC:
    node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  case cdk::TYPE_INT:
    break;
  default:
    throw std::string("wrong type in condition of if expression");
  }
}

//---------------------------------------------------------------------------
void til::type_checker::do_block_node(til::block_node *const node, int lvl) {
  node->declarations()->accept(this, lvl + 2);
  node->instructions()->accept(this, lvl + 2);
  // TODO: Maybe empty block
}

void til::type_checker::do_return_node(til::return_node *const node, int lvl) {
  node->returnval()->accept(this, lvl + 2);
}

void til::type_checker::do_stop_node(til::stop_node *const node, int lvl) {
  // EMPTY
}

void til::type_checker::do_next_node(til::next_node *const node, int lvl) {
  // EMPTY
}

void til::type_checker::do_index_node(til::index_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->base()->accept(this, lvl + 2);
  if (!node->base()->is_typed(cdk::TYPE_POINTER)) {
    throw std::string("wrong type in left argument of index expression");
  }
  if (node->type() != nullptr && node->type()->name() != cdk::TYPE_UNSPEC) {

    node->ind()->accept(this, lvl + 2);
    if (!node->ind()->is_typed(cdk::TYPE_INT)) {
      if (node->ind()->is_typed(cdk::TYPE_UNSPEC)) {
        node->ind()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      } else {
        throw std::string("wrong type in right argument of index expression");
      }
    }

    auto basetype = cdk::reference_type::cast(node->base()->type());

    // TODO check if basetype is unspec

    node->type(basetype->referenced());
  }
}

void til::type_checker::do_address_of_node(til::address_of_node *const node,
                                           int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 2);

  if (node->lvalue()->is_typed(cdk::TYPE_POINTER)) {
    auto pointer = cdk::reference_type::cast(node->lvalue()->type());
    if (pointer->referenced()->name() == cdk::TYPE_VOID) {
      node->type(cdk::reference_type::create(
          4, cdk::primitive_type::create(4, cdk::TYPE_INT)));
    }
    node->type(cdk::reference_type::create(4, node->lvalue()->type()));
  }

  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

void til::type_checker::do_nullptr_node(til::nullptr_node *const node,
                                        int lvl) {}

void til::type_checker::do_sizeof_node(til::sizeof_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

void til::type_checker::do_var_declaration_node(
    til::var_declaration_node *const node, int lvl) {
  ASSERT_UNSPEC;

  if (node->type() != nullptr) {
    if (node->init()->type() != nullptr) {
      node->init()->accept(this, lvl + 2);

      if (node->init()->is_typed(cdk::TYPE_UNSPEC)) {
        if (node->is_typed(cdk::TYPE_DOUBLE)) {
          node->init()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
        } else if (node->is_typed(cdk::TYPE_INT)) {
          node->init()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        } else {
          throw std::string("wrong type in initialization of variable");
        }

        // Check if types are the same
        if (node->type()->name() != node->init()->type()->name()) {
          throw std::string("wrong type in initialization of variable");
        }

      } else if (node->init()->is_typed(cdk::TYPE_POINTER) &&
                 node->is_typed(cdk::TYPE_POINTER)) {
        auto node_ptr = cdk::reference_type::cast(node->type());
        auto init_ptr = cdk::reference_type::cast(node->init()->type());

        if (node_ptr->referenced()->name() == cdk::TYPE_VOID ||
            init_ptr->referenced()->name() == cdk::TYPE_VOID) {
          node->init()->type(node->type());
        } else if (init_ptr->referenced()->name() == cdk::TYPE_UNSPEC) {
          node->init()->type(node->type());
        }

        // Check if the types are the same
        if (node_ptr->referenced()->name() != init_ptr->referenced()->name() ||
            (node_ptr->referenced()->name() == cdk::TYPE_DOUBLE &&
             init_ptr->referenced()->name() == cdk::TYPE_INT)) {
          throw std::string("wrong type in initialization of variable");
        }
      }
    } else {
      throw std::string("wrong type in initialization of variable");
    }
  } else { // Should infer type from initialization
    node->init()->accept(this, lvl + 2);

    if (node->init()->is_typed(cdk::TYPE_UNSPEC)) {
      node->init()->type(
          cdk::primitive_type::create(4, cdk::TYPE_INT)); // Why is this int?
    } else if (node->init()->is_typed(cdk::TYPE_POINTER)) {
      auto init_ptr = cdk::reference_type::cast(node->init()->type());
      if (init_ptr->referenced()->name() == cdk::TYPE_VOID) {
        throw std::string("wrong type in initialization of variable");
      } else if (init_ptr->referenced()->name() == cdk::TYPE_UNSPEC) {
        node->init()->type(cdk::reference_type::create(
            4, cdk::primitive_type::create(4, cdk::TYPE_INT)));
      }
    }

    node->type(node->init()->type());
  }

  if (node->qualifier() == tEXTERNAL && !node->is_typed(cdk::TYPE_FUNCTIONAL)) {
    throw std::string("External variable must be a function");
  }

  auto symbol = create_symbol(node->type(), node->name(), 0);

  if (_symtab.insert(node->name(), symbol)) {
    _parent->set_new_symbol(symbol);
    return;
  }

  // TODO: Check symbol redeclaration
  // auto previous = _symtab.find(node->name());

  throw std::string("variable redefinition: " + node->name());
}

void til::type_checker::do_function_node(til::function_node *const node,
                                         int lvl) {}

void til::type_checker::do_call_node(til::call_node *const node, int lvl) {}

void til::type_checker::do_alloc_node(til::alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  switch (node->argument()->type()->name()) {
  case cdk::TYPE_UNSPEC:
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->type(cdk::reference_type::create(
        4, cdk::primitive_type::create(
               4, cdk::TYPE_INT))); // TODO Check if should be reference type
    break;
  case cdk::TYPE_INT: {
    node->type(cdk::reference_type::create(
        4, cdk::primitive_type::create(
               4, cdk::TYPE_INT))); // TODO Check if should be reference type
    break;
  }
  default:
    throw std::string("wrong type in argument of alloc expression");
  }
}