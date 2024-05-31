#include "targets/type_checker.h"
#include ".auto/all_nodes.h" // automatically generated
#include "til_parser.tab.h"
#include <cdk/types/primitive_type.h>
#include <string>

#define ASSERT_UNSPEC                                                          \
  {                                                                            \
    if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC))          \
      return;                                                                  \
  }

std::string til::to_string(std::shared_ptr<cdk::basic_type> type) {
  if (type->name() == cdk::TYPE_VOID) {
    return "void";
  }

  if (type->name() == cdk::TYPE_INT) {
    return "int";
  }

  if (type->name() == cdk::TYPE_DOUBLE) {
    return "double";
  }

  if (type->name() == cdk::TYPE_STRING) {
    return "string";
  }

  if (type->name() == cdk::TYPE_UNSPEC) {
    return "unspec";
  }

  if (type->name() == cdk::TYPE_POINTER) {
    auto ptr = std::dynamic_pointer_cast<cdk::reference_type>(type);
    return til::to_string(ptr->referenced()) + '!';
  }

  if (type->name() == cdk::TYPE_FUNCTIONAL) {
    auto ptr = std::dynamic_pointer_cast<cdk::functional_type>(type);
    std::string ret = "(";
    for (size_t i = 0; i < ptr->input_length(); i++) {
      ret += til::to_string(ptr->input(i));
      if (i != ptr->input_length() - 1)
        ret += ", ";
    }
    ret += ") -> " + til::to_string(ptr->output(0));
    return ret;
  }

  throw std::string("Invalid node type");
}

bool til::deep_type_cmp(std::shared_ptr<cdk::basic_type> left,
                        std::shared_ptr<cdk::basic_type> right) {
  if (left->name() == cdk::TYPE_FUNCTIONAL &&
      right->name() == cdk::TYPE_FUNCTIONAL) {
    // Compare function types
    auto left_func = cdk::functional_type::cast(left);
    auto right_func = cdk::functional_type::cast(right);

    if (left_func->input_length() != right_func->input_length()) {
      return false;
    }

    for (size_t i = 0; i < left_func->input_length(); i++) {
      if (!til::deep_type_cmp(left_func->input(i), right_func->input(i))) {
        return false;
      }
    }

    return til::deep_type_cmp(left_func->output(0), right_func->output(0));
  }

  if (left->name() == cdk::TYPE_POINTER && right->name() == cdk::TYPE_POINTER) {
    // Compare recursivly
    return til::deep_type_cmp(cdk::reference_type::cast(left)->referenced(),
                              cdk::reference_type::cast(right)->referenced());
  }

  return left->name() == right->name();
}

static std::pair<std::shared_ptr<cdk::basic_type>,
                 std::shared_ptr<cdk::basic_type>>
unify_types(std::shared_ptr<cdk::basic_type> src_type,
            std::shared_ptr<cdk::basic_type> dest_type) {

  switch (src_type->name()) {
  case cdk::TYPE_UNSPEC: {
    if (dest_type->name() == cdk::TYPE_POINTER &&
        cdk::reference_type::cast(dest_type)->referenced()->name() ==
            cdk::TYPE_VOID) {
      return {cdk::reference_type::create(4, src_type), dest_type};
    }
    return {dest_type, dest_type};
  }

  case cdk::TYPE_POINTER: {
    if (dest_type->name() == cdk::TYPE_POINTER) {
      auto src_referenced = cdk::reference_type::cast(src_type)->referenced();
      auto dest_referenced = cdk::reference_type::cast(dest_type)->referenced();

      if ((src_referenced->name() == cdk::TYPE_POINTER &&
           dest_referenced->name() == cdk::TYPE_POINTER) ||
          src_referenced->name() == cdk::TYPE_UNSPEC ||
          dest_referenced->name() == cdk::TYPE_UNSPEC) {
        auto [new_src_referenced, new_dest_referenced] =
            unify_types(src_referenced, dest_referenced);

        if (new_src_referenced.get() != src_referenced.get()) {
          src_type = cdk::reference_type::create(4, new_src_referenced);
        }

        if (new_dest_referenced.get() != dest_referenced.get()) {
          dest_type = cdk::reference_type::create(4, new_dest_referenced);
        }
      } else if (src_referenced->name() != cdk::TYPE_VOID &&
                 dest_referenced->name() != cdk::TYPE_VOID) {
        if (!til::deep_type_cmp(src_referenced, dest_referenced)) {
          throw std::string("cannot unify pointers to different types");
        }
      }

      return {src_type, dest_type};
    }
    break;
  }

  case cdk::TYPE_FUNCTIONAL: {
    if (dest_type->name() == cdk::TYPE_FUNCTIONAL) {
      auto src_func = cdk::functional_type::cast(src_type);
      auto dest_func = cdk::functional_type::cast(dest_type);

      if (src_func->input_length() != dest_func->input_length()) {
        throw std::string("cannot unify functions with different arguments");
      }

      std::vector<std::shared_ptr<cdk::basic_type>> src_inputs;
      std::vector<std::shared_ptr<cdk::basic_type>> dest_inputs;
      bool src_input_changed = false;
      bool dest_input_changed = false;

      for (std::size_t i = 0; i < src_func->input_length(); ++i) {
        auto [new_dest, new_src] =
            unify_types(dest_func->input(i), src_func->input(i));
        src_inputs.push_back(new_src);
        dest_inputs.push_back(new_dest);
        src_input_changed |= new_src.get() != src_func->input(i).get();
        dest_input_changed |= new_dest.get() != dest_func->input(i).get();
      }

      auto [src_output, dest_output] =
          unify_types(src_func->output(0), dest_func->output(0));
      if (src_output.get() != src_func->output(0).get() || src_input_changed) {
        src_type = cdk::functional_type::create(src_inputs, src_output);
      }
      if (dest_output.get() != dest_func->output(0).get() ||
          dest_input_changed) {
        dest_type = cdk::functional_type::create(dest_inputs, dest_output);
      }

      return {src_type, dest_type};
    }
    break;
  }

  default:
    break;
  }

  if (src_type->name() == dest_type->name() &&
      (dest_type->name() == cdk::TYPE_VOID ||
       dest_type->name() == cdk::TYPE_INT ||
       dest_type->name() == cdk::TYPE_DOUBLE ||
       dest_type->name() == cdk::TYPE_STRING)) {
    return {src_type, dest_type};
  }

  if (src_type->name() == cdk::TYPE_INT &&
      dest_type->name() == cdk::TYPE_DOUBLE) {
    return {src_type, dest_type};
  }

  if (dest_type->name() == cdk::TYPE_UNSPEC) {
    if (src_type->name() == cdk::TYPE_POINTER &&
        cdk::reference_type::cast(src_type)->referenced()->name() ==
            cdk::TYPE_VOID) {
      return {src_type, cdk::reference_type::create(4, dest_type)};
    }

    return {src_type, src_type};
  }

  throw std::string("cannot cast '" + til::to_string(src_type) + "' to '" +
                    til::to_string(dest_type) + "'");
}

static std::shared_ptr<cdk::basic_type>
convert_unspecified_to_int(std::shared_ptr<cdk::basic_type> type) {
  switch (type->name()) {
  case cdk::TYPE_UNSPEC:
    return cdk::primitive_type::create(4, cdk::TYPE_INT);

  case cdk::TYPE_POINTER: {
    auto reference = std::dynamic_pointer_cast<cdk::reference_type>(type);
    auto referenced = convert_unspecified_to_int(reference->referenced());
    if (reference->referenced().get() != referenced.get()) {
      return cdk::reference_type::create(4, referenced);
    } else {
      return type;
    }
  }

  case cdk::TYPE_FUNCTIONAL: {
    auto functional = std::dynamic_pointer_cast<cdk::functional_type>(type);

    std::vector<std::shared_ptr<cdk::basic_type>> inputs;
    bool input_changed = false;
    for (auto &input : functional->input()->components()) {
      auto new_input = convert_unspecified_to_int(input);
      input_changed |= new_input.get() != input.get();
      inputs.push_back(new_input);
    }

    auto output = convert_unspecified_to_int(functional->output(0));
    if (output.get() != functional->output(0).get() || input_changed) {
      return cdk::functional_type::create(inputs, output);
    } else {
      return type;
    }
  }

  default:
    return type;
  }
}

std::shared_ptr<cdk::basic_type> til::type_checker::unify_node_with_type(
    cdk::typed_node *const node, std::shared_ptr<cdk::basic_type> target_type,
    int level) {
  auto current_type = node->type();

  try {
    auto [new_current_type, new_target_type] =
        unify_types(current_type, target_type);
    if (new_current_type.get() != current_type.get()) {
      node->type(new_current_type);
      propagate(node, level + 2);
      if (_isTesting) {
        node->type(current_type);
      }
    }

    return new_target_type;
  } catch (std::string &e) {
    node->type(current_type);
    e = "failed to unify '" + node->label() + "' from type '" +
        til::to_string(current_type) + "' to '" + til::to_string(target_type) +
        "'\n" + e;
    throw e;
  }
}

void til::type_checker::unify_nodes(cdk::typed_node *const from_node,
                                    cdk::typed_node *const to_node, int level) {
  auto target_type = to_node->type();

  try {
    auto new_target_type = unify_node_with_type(from_node, target_type, level);
    if (new_target_type.get() != target_type.get()) {
      to_node->type(new_target_type);
      propagate(to_node, level + 2);
      if (_isTesting) {
        to_node->type(target_type);
      }
    }
  } catch (std::string &) {
    to_node->type(target_type);
    throw;
  }
}

bool til::type_checker::test_unify_node_with_type(
    cdk::typed_node *const node, std::shared_ptr<cdk::basic_type> target_type,
    int level) {
  _isTesting = true;
  try {
    unify_node_with_type(node, target_type, level);
    _isTesting = false;
    return true;
  } catch (std::string &) {
    _isTesting = false;
    return false;
  }
}

void til::type_checker::convert_node_to_int(cdk::typed_node *const node,
                                            int level) {
  unify_node_with_type(node, convert_unspecified_to_int(node->type()), level);
}

void til::type_checker::propagate(cdk::typed_node *const node, int level) {
  if (_isPropagating) {
    node->accept(this, level);
    return;
  }

  _isPropagating = true;
  node->accept(this, level);
  _isPropagating = false;
}

//---------------------------------------------------------------------------

void til::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

void til::type_checker::do_unary_minus_node(cdk::unary_minus_node *const node,
                                            int lvl) {
  processUnaryExpression(node, lvl);
}

void til::type_checker::do_unary_plus_node(cdk::unary_plus_node *const node,
                                           int lvl) {
  processUnaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::do_sequence_node(cdk::sequence_node *const node,
                                         int lvl) {
  for (auto &node : node->nodes()) {
    node->accept(this, lvl + 2);
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_integer_node(cdk::integer_node *const node,
                                        int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void til::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

void til::type_checker::do_nullptr_node(til::nullptr_node *const node,
                                        int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(
      4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

//---------------------------------------------------------------------------

void til::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);
  unify_node_with_type(node->argument(),
                       cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
  node->type(node->argument()->type());
}

void til::type_checker::processUnaryExpression(
    cdk::unary_operation_node *const node, int lvl) {
  if (_isPropagating) {
    unify_node_with_type(node->argument(), node->type(), lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_DOUBLE) ||
      node->argument()->is_typed(cdk::TYPE_INT) ||
      node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(node->argument()->type());
  } else {
    throw std::string("invalid operand type for unary expression");
  }
}

void til::type_checker::do_sizeof_node(til::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);
  unify_node_with_type(node->argument(),
                       convert_unspecified_to_int(node->argument()->type()),
                       lvl + 2);

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_alloc_node(til::alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  unify_node_with_type(node->argument(),
                       cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);

  node->type(cdk::reference_type::create(
      4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

//---------------------------------------------------------------------------

void til::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  if (_isPropagating) {
    if (node->is_typed(cdk::TYPE_POINTER)) {
      // Either U + U, U + I or I + U.
      if (node->left()->is_typed(cdk::TYPE_INT)) {
        // I + U = P (U <- P).
        unify_node_with_type(node->right(), node->type(), lvl + 2);
      } else if (node->right()->is_typed(cdk::TYPE_INT)) {
        // U + I = P (U <- P).
        unify_node_with_type(node->left(), node->type(), lvl + 2);
      } else {
        // U1 + U2 = P.
        // Either U1 or U2 must unify with I - we check which is possible.
        if (test_unify_node_with_type(
                node->left(), cdk::primitive_type::create(4, cdk::TYPE_INT),
                lvl + 2) &&
            test_unify_node_with_type(node->right(), node->type(), lvl + 2)) {
          unify_node_with_type(node->left(),
                               cdk::primitive_type::create(4, cdk::TYPE_INT),
                               lvl + 2);
          unify_node_with_type(node->right(), node->type(), lvl + 2);
        } else {
          unify_node_with_type(node->left(), node->type(), lvl + 2);
          unify_node_with_type(node->right(),
                               cdk::primitive_type::create(4, cdk::TYPE_INT),
                               lvl + 2);
        }
      }
    } else {
      // Both arguments unify with the expression type, removing any previous
      // unspecified type.
      unify_node_with_type(node->left(), node->type(), lvl + 2);
      unify_node_with_type(node->right(), node->type(), lvl + 2);
    }

    return;
  }

  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  // Given x + y = z
  if (node->left()->is_typed(cdk::TYPE_POINTER)) {
    // If x = P, then y <- I, z = P
    unify_node_with_type(
        node->right(), cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
    node->type(node->left()->type()); // Results in a pointer.
  } else if (node->right()->is_typed(cdk::TYPE_POINTER)) {
    // Else if y = P, then x <- I, z = P
    unify_node_with_type(
        node->left(), cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
    node->type(node->right()->type()); // Results in a pointer.
  } else if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    // Else if, x = D, then y <- D, z = D
    unify_node_with_type(node->right(),
                         cdk::primitive_type::create(8, cdk::TYPE_DOUBLE),
                         lvl + 2);
    node->type(node->left()->type()); // Results in a double.
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    // Else if, y = D, then x <- D, z = D
    unify_node_with_type(node->left(),
                         cdk::primitive_type::create(8, cdk::TYPE_DOUBLE),
                         lvl + 2);
    node->type(node->right()->type()); // Results in a double.
  } else if (node->left()->is_typed(cdk::TYPE_INT) &&
             node->right()->is_typed(cdk::TYPE_INT)) {
    // Else if, x = I and y = I, then z = I
    node->type(node->left()->type()); // Results in an int.
  } else if (node->left()->is_typed(cdk::TYPE_INT) &&
             node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    // Else if, x = I and y = U, then z = U
    node->type(node->right()->type()); // Remains unspecified - leave decision
                                       // between int and double to the parent.
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) &&
             node->right()->is_typed(cdk::TYPE_INT)) {
    // Else if, x = U and y = I, then z = U
    node->type(node->left()->type()); // Remains unspecified - leave decision
                                      // between int and double to the parent.
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) &&
             node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    // Else if, x = U and y = U, then z = U
    node->type(node->left()->type()); // Remains unspecified - leave decision
                                      // between int and double to the parent.
  } else {
    throw std::string("invalid operand types for addition expression");
  }
}

void til::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  if (_isPropagating) {
    if (node->is_typed(cdk::TYPE_POINTER)) {
      // X - Y = P, X <- P, Y <- I
      unify_node_with_type(node->left(), node->type(), lvl + 2);
      unify_node_with_type(node->right(),
                           cdk::primitive_type::create(4, cdk::TYPE_INT),
                           lvl + 2);
    } else if (node->is_typed(cdk::TYPE_INT)) {
      if (node->left()->is_typed(cdk::TYPE_POINTER)) {
        // P - U = I, U <- P
        unify_node_with_type(
            node->left(),
            cdk::reference_type::create(
                4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)),
            lvl + 2);
        unify_nodes(node->left(), node->right(), lvl + 2);
        unify_node_with_type(node->left(),
                             convert_unspecified_to_int(node->left()->type()),
                             lvl + 2);
        unify_node_with_type(node->right(),
                             convert_unspecified_to_int(node->right()->type()),
                             lvl + 2);
      } else {
        // U - U = I, U <- I
        unify_node_with_type(node->left(), node->type(), lvl + 2);
        unify_node_with_type(node->right(), node->type(), lvl + 2);
      }
    } else {
      // Both arguments unify with the expression type, removing any previous
      // unspecified type.
      unify_node_with_type(node->left(), node->type(), lvl + 2);
      unify_node_with_type(node->right(), node->type(), lvl + 2);
    }
  }

  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  // Given x - y = z
  if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    // If x = D, then y <- D, z = D
    unify_node_with_type(node->right(),
                         cdk::primitive_type::create(8, cdk::TYPE_DOUBLE),
                         lvl + 2);
    node->type(node->left()->type()); // Results in a double.
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    // Else if y = D, then x <- D, z = D
    unify_node_with_type(node->left(),
                         cdk::primitive_type::create(8, cdk::TYPE_DOUBLE),
                         lvl + 2);
    node->type(node->right()->type()); // Results in a double.
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) &&
             node->right()->is_typed(cdk::TYPE_POINTER)) {
    // Else if x = U and y = P, then x <- P, z = I
    unify_node_with_type(node->left(), node->right()->type(), lvl + 2);
    node->type(
        cdk::primitive_type::create(4, cdk::TYPE_INT)); // Results in an int.
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) ||
             node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    // Else if x = U or y = U, then z = U
    node->type(cdk::primitive_type::create(
        0,
        cdk::TYPE_UNSPEC)); // Remains unspecified - leave decision to parent.
  } else if (node->left()->is_typed(cdk::TYPE_INT)) {
    // Else if x = I, then y <- I, z = I
    unify_node_with_type(
        node->right(), cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
    node->type(node->left()->type()); // Results in an int.
  } else if (node->left()->is_typed(cdk::TYPE_POINTER) &&
             node->right()->is_typed(cdk::TYPE_INT)) {
    // Else if x = P and y = I, then z = P
    node->type(node->left()->type()); // Results in a pointer.
  } else if (node->left()->is_typed(cdk::TYPE_POINTER)) {
    // Else if x = P, then x <-> y, z = I
    unify_nodes(node->left(), node->right(), lvl + 2);
    node->type(
        cdk::primitive_type::create(4, cdk::TYPE_INT)); // Results in an int.
  } else {
    throw std::string("invalid operand types for subtraction expression");
  }
}

void til::type_checker::processMulExpression(
    cdk::binary_operation_node *const node, int lvl) {
  if (_isPropagating) {
    if (!node->is_typed(cdk::TYPE_INT) && !node->is_typed(cdk::TYPE_DOUBLE)) {
      throw std::string("invalid operand types for multiplication expression");
    }

    unify_node_with_type(node->left(), node->type(), lvl + 2);
    unify_node_with_type(node->right(), node->type(), lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    unify_node_with_type(node->right(),
                         cdk::primitive_type::create(8, cdk::TYPE_DOUBLE),
                         lvl + 2);
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    unify_node_with_type(node->left(),
                         cdk::primitive_type::create(8, cdk::TYPE_DOUBLE),
                         lvl + 2);
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC) ||
             node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(
        0,
        cdk::TYPE_UNSPEC)); // Keeps the type unspecified. Should be resolved by
                            // parent
  } else {
    unify_node_with_type(
        node->left(), cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
    unify_node_with_type(
        node->right(), cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
}

void til::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processMulExpression(node, lvl);
}
void til::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processMulExpression(node, lvl);
}

void til::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));

  unify_node_with_type(node->left(),
                       cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
  unify_node_with_type(node->right(),
                       cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
}

void til::type_checker::processCmpExpression(
    cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));

  if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    unify_node_with_type(node->right(),
                         cdk::primitive_type::create(8, cdk::TYPE_DOUBLE),
                         lvl + 2);
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    unify_node_with_type(node->left(),
                         cdk::primitive_type::create(8, cdk::TYPE_DOUBLE),
                         lvl + 2);
  } else {
    unify_node_with_type(
        node->left(), cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
    unify_node_with_type(
        node->right(), cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
  }
}

void til::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processCmpExpression(node, lvl);
}
void til::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processCmpExpression(node, lvl);
}
void til::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processCmpExpression(node, lvl);
}
void til::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processCmpExpression(node, lvl);
}

void til::type_checker::processEqExpression(
    cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));

  if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    unify_node_with_type(node->right(),
                         cdk::primitive_type::create(8, cdk::TYPE_DOUBLE),
                         lvl + 2);
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    unify_node_with_type(node->left(),
                         cdk::primitive_type::create(8, cdk::TYPE_DOUBLE),
                         lvl + 2);
  } else if (node->left()->is_typed(cdk::TYPE_POINTER) ||
             node->right()->is_typed(cdk::TYPE_POINTER)) {
    unify_nodes(node->left(), node->right(), lvl + 2);
    convert_node_to_int(node->left(), lvl + 2);
    convert_node_to_int(node->right(), lvl + 2);
  } else {
    unify_node_with_type(
        node->left(), cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
    unify_node_with_type(
        node->right(), cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
  }
}

void til::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processEqExpression(node, lvl);
}
void til::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processEqExpression(node, lvl);
}

void til::type_checker::processLogicalExpression(
    cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));

  unify_node_with_type(node->left(),
                       cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
  unify_node_with_type(node->right(),
                       cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
}

void til::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processLogicalExpression(node, lvl);
}
void til::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processLogicalExpression(node, lvl);
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node *const node,
                                         int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<til::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->node()->type());
  } else {
    throw "undeclared variable '" + id + "'";
  }
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  if (_isPropagating) {
    unify_node_with_type(node->lvalue(), node->type(), lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl);
  node->type(node->lvalue()->type());
}

void til::type_checker::do_assignment_node(cdk::assignment_node *const node,
                                           int lvl) {
  if (_isPropagating) {
    unify_node_with_type(node->lvalue(), node->type(), lvl + 2);
    unify_nodes(node->rvalue(), node->lvalue(), lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 2);
  node->rvalue()->accept(this, lvl + 2);

  unify_nodes(node->rvalue(), node->lvalue(), lvl + 2);

  node->type(node->lvalue()->type());
}

//---------------------------------------------------------------------------

void til::type_checker::do_block_node(til::block_node *const node, int lvl) {
  _symtab.push();
  node->declarations()->accept(this, lvl + 2);
  node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}

void til::type_checker::do_var_declaration_node(
    til::var_declaration_node *const node, int lvl) {
  if (node->init()) {
    node->init()->accept(this, lvl + 2);

    if (node->type() == nullptr) {
      // Figure out the type of the variable through the initializer. Default
      // unspecs to int.
      node->type(convert_unspecified_to_int(node->init()->type()));
    }

    unify_node_with_type(node->init(), node->type(), lvl + 2);
    convert_node_to_int(node->init(), lvl + 2);

    if (node->is_typed(cdk::TYPE_VOID)) {
      throw std::string("var cannot be void");
    }
  }

  auto symbol = _symtab.find_local(node->name());
  if (symbol != nullptr) {
    if (symbol->node() == node) {
      return;
    }

    if (!til::deep_type_cmp(node->type(), symbol->node()->type())) {
      throw std::string("var redeclared with different type");
    }

    if (node->qualifier() == tEXTERNAL ||
        symbol->node()->qualifier() == tEXTERNAL) {
      throw std::string("var redeclared with different qualifier");
    }

    if (symbol->node()->qualifier() == tFORWARD) {
      symbol->node(node);
      return;
    }

    if (node->qualifier() == tFORWARD) {
      return;
    }

    throw std::string("var redeclared on the same scope");
  }

  if (node->qualifier() == tEXTERNAL) {
    // Foreign variables must be functions.
    if (!node->is_typed(cdk::TYPE_FUNCTIONAL)) {
      throw std::string("foreign variable must be a function");
    }
  }

  _symtab.insert(node->name(), std::make_shared<til::symbol>(node));
}

void til::type_checker::do_index_node(til::index_node *const node, int lvl) {
  if (_isPropagating) {
    // The base node must be a pointer to this node.
    unify_node_with_type(node->base(),
                         cdk::reference_type::create(4, node->type()), lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->base()->accept(this, lvl + 2);
  node->ind()->accept(this, lvl + 2);

  // Base must be a pointer to something, so we unify it with a pointer to
  // unspec.
  auto newType = unify_node_with_type(
      node->base(),
      cdk::reference_type::create(
          4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)),
      lvl + 2);

  // The type is what the referenced unspec was unified to.
  node->type(cdk::reference_type::cast(newType)->referenced());

  // The index must unify to an integer.
  unify_node_with_type(node->ind(),
                       cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
}

void til::type_checker::do_address_of_node(til::address_of_node *const node,
                                           int lvl) {
  if (_isPropagating) {
    // The lvalue node must be the referenced type of this node.
    unify_node_with_type(node->lvalue(),
                         cdk::reference_type::cast(node->type())->referenced(),
                         lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 2);

  // No matter what, the type of this node is always a pointer to the lvalue.
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

void til::type_checker::do_evaluation_node(til::evaluation_node *const node,
                                           int lvl) {
  node->argument()->accept(this, lvl + 2);

  // We must unify the argument with its type but where all instances of
  // TYPE_UNSPEC are replaced with TYPE_INT.
  convert_node_to_int(node->argument(), lvl + 2);
}

void til::type_checker::do_print_node(til::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);

  for (const auto &argument : node->arguments()->nodes()) {
    auto typed = static_cast<cdk::typed_node *>(argument);

    // We only unify to int if the type is still unspecified.
    if (typed->is_typed(cdk::TYPE_UNSPEC)) {
      unify_node_with_type(typed, cdk::primitive_type::create(4, cdk::TYPE_INT),
                           lvl + 2);
    } else if (!typed->is_typed(cdk::TYPE_INT) &&
               !typed->is_typed(cdk::TYPE_DOUBLE) &&
               !typed->is_typed(cdk::TYPE_STRING) &&
               !typed->is_typed(cdk::TYPE_VOID)) {
      throw std::string("wrong type in print expression");
    }
  }
}

void til::type_checker::do_read_node(til::read_node *const node, int lvl) {
  if (!_isPropagating) {
    ASSERT_UNSPEC;

    node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
  } else if (!node->is_typed(cdk::TYPE_INT) &&
             !node->is_typed(cdk::TYPE_DOUBLE)) {
    throw std::string("input only supports integer or real numbers");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_program_node(til::program_node *const node,
                                        int lvl) {
  _symtab.push();
  node->block()->accept(this, lvl + 2);
  _symtab.pop();
}

void til::type_checker::do_function_node(til::function_node *const node,
                                         int lvl) {
  auto oldFunctionType = _functionType;
  _functionType = cdk::functional_type::cast(node->type());

  _symtab.push();
  node->args()->accept(this, lvl + 2);
  node->block()->accept(this, lvl + 2);
  _symtab.pop();

  _functionType = oldFunctionType;
}

void til::type_checker::do_call_node(til::call_node *const node, int lvl) {
  std::shared_ptr<cdk::functional_type> functionType;

  if (_isPropagating) {
    functionType = cdk::functional_type::cast(node->function()->type());
    auto expectedFunctionType = cdk::functional_type::create(
        functionType->input()->components(), node->type());
    unify_node_with_type(node->function(), expectedFunctionType, lvl + 2);
    return;
  }

  ASSERT_UNSPEC;

  node->args()->accept(this, lvl + 2);

  if (node->function() == nullptr) {
    if (_functionType == nullptr) {
      throw std::string("cannot call recurse outside of function definition");
    }

    functionType = _functionType;

    if (functionType->input_length() != node->args()->size()) {
      throw std::string("wrong number of arguments in recursive call");
    }

    // Unify the arguments to the expected input types.
    for (std::size_t i = 0; i < node->args()->size(); ++i) {
      auto typed = static_cast<cdk::typed_node *>(node->args()->node(i));
      unify_node_with_type(typed, functionType->input(i), lvl + 2);
      convert_node_to_int(typed, lvl + 2);
    }
  } else {
    node->function()->accept(this, lvl + 2);

    // Create the expected function type.
    std::vector<std::shared_ptr<cdk::basic_type>> inputs;
    for (auto *node : node->args()->nodes()) {
      inputs.push_back(static_cast<cdk::typed_node *>(node)->type());
    }

    // Unify the function node with the expected type.
    auto expectedFunctionType = cdk::functional_type::create(
        inputs, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
    functionType = cdk::functional_type::cast(
        unify_node_with_type(node->function(), expectedFunctionType, lvl + 2));

    // Unify the arguments to the expected input types.
    bool changed = false;
    for (std::size_t i = 0; i < node->args()->size(); ++i) {
      auto typed = static_cast<cdk::typed_node *>(node->args()->node(i));
      inputs[i] = unify_node_with_type(
          typed, convert_unspecified_to_int(functionType->input(i)), lvl + 2);
      changed |= inputs[i] != functionType->input(i);
    }

    if (changed) {
      unify_node_with_type(
          node->function(),
          cdk::functional_type::create(inputs, functionType->output(0)),
          lvl + 2);
    }
  }

  node->type(functionType->output(0));
}

void til::type_checker::do_return_node(til::return_node *const node, int lvl) {
  auto outputType = _functionType == nullptr
                        ? cdk::primitive_type::create(4, cdk::TYPE_INT)
                        : _functionType->output(0);

  if (outputType->name() != cdk::TYPE_VOID) {
    if (node->returnval() == nullptr) {
      throw std::string("non-void function must return a value");
    }

    node->returnval()->accept(this, lvl + 2);
    unify_node_with_type(node->returnval(), outputType, lvl + 2);
    convert_node_to_int(node->returnval(), lvl + 2);
  } else if (node->returnval() != nullptr) {
    throw std::string("void function cannot return a value");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_loop_node(til::loop_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 2);
  node->block()->accept(this, lvl + 2);
  unify_node_with_type(node->condition(),
                       cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
}

void til::type_checker::do_stop_node(til::stop_node *const node, int lvl) {
  // EMPTY
}

void til::type_checker::do_next_node(til::next_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_if_node(til::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 2);
  node->block()->accept(this, lvl + 2);
  unify_node_with_type(node->condition(),
                       cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
}

void til::type_checker::do_if_else_node(til::if_else_node *const node,
                                        int lvl) {
  node->condition()->accept(this, lvl + 2);
  node->thenblock()->accept(this, lvl + 2);
  node->elseblock()->accept(this, lvl + 2);
  unify_node_with_type(node->condition(),
                       cdk::primitive_type::create(4, cdk::TYPE_INT), lvl + 2);
}
