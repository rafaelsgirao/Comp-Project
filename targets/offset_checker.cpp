#include "offset_checker.h"
#include ".auto/all_nodes.h" // automatically generated
#include "type_checker.h"

void til::offset_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_sequence_node(cdk::sequence_node *const node,
                                           int lvl) {
  for (auto &node : node->nodes()) {
    node->accept(this, lvl);
  }
}

void til::offset_checker::do_integer_node(cdk::integer_node *const node,
                                          int lvl) {
  // EMPTY
}
void til::offset_checker::do_double_node(cdk::double_node *const node,
                                         int lvl) {
  // EMPTY
}
void til::offset_checker::do_string_node(cdk::string_node *const node,
                                         int lvl) {
  // EMPTY
}
void til::offset_checker::do_nullptr_node(til::nullptr_node *const node,
                                          int lvl) {
  // EMPTY
}
void til::offset_checker::do_not_node(cdk::not_node *const node, int lvl) {
  // EMPTY
}

void til::offset_checker::do_sizeof_node(til::sizeof_node *const node,
                                         int lvl) {
  // EMPTY
}
void til::offset_checker::do_add_node(cdk::add_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_div_node(cdk::div_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_le_node(cdk::le_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_and_node(cdk::and_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_or_node(cdk::or_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_variable_node(cdk::variable_node *const node,
                                           int lvl) {
  // EMPTY
}
void til::offset_checker::do_unary_minus_node(cdk::unary_minus_node *const node,
                                              int lvl) {
  // EMPTY
}
void til::offset_checker::do_unary_plus_node(cdk::unary_plus_node *const node,
                                             int lvl) {
  // EMPTY
}
void til::offset_checker::do_rvalue_node(cdk::rvalue_node *const node,
                                         int lvl) {
  // EMPTY
}
void til::offset_checker::do_assignment_node(cdk::assignment_node *const node,
                                             int lvl) {
  // EMPTY
}
void til::offset_checker::do_block_node(til::block_node *const node, int lvl) {
  _symtab.push();
  node->declarations()->accept(this, lvl);
  node->instructions()->accept(this, lvl);
  _symtab.pop();
}
void til::offset_checker::do_var_declaration_node(
    til::var_declaration_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _size += node->type()->size();
}
void til::offset_checker::do_index_node(til::index_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_address_of_node(til::address_of_node *const node,
                                             int lvl) {
  // EMPTY
}
void til::offset_checker::do_evaluation_node(til::evaluation_node *const node,
                                             int lvl) {
  // EMPTY
}
void til::offset_checker::do_print_node(til::print_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_read_node(til::read_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_function_node(til::function_node *const node,
                                           int lvl) {
  // EMPTY
}
void til::offset_checker::do_call_node(til::call_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_return_node(til::return_node *const node,
                                         int lvl) {
  // EMPTY
}
void til::offset_checker::do_loop_node(til::loop_node *const node, int lvl) {
  node->block()->accept(this, lvl);
}
void til::offset_checker::do_stop_node(til::stop_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_next_node(til::next_node *const node, int lvl) {
  // EMPTY
}
void til::offset_checker::do_if_node(til::if_node *const node, int lvl) {
  node->block()->accept(this, lvl);
}
void til::offset_checker::do_if_else_node(til::if_else_node *const node,
                                          int lvl) {
  node->thenblock()->accept(this, lvl);
  node->elseblock()->accept(this, lvl);
}
void til::offset_checker::do_program_node(til::program_node *const node,
                                          int lvl) {
  // EMPTY
}
void til::offset_checker::do_alloc_node(til::alloc_node *const node, int lvl) {
  // EMPTY
}
