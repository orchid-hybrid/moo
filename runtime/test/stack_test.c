#include <stdio.h>
#include <stdlib.h>

#include "scm.h"
#include "symbols.c"
#include "stack.c"

scm sym(char *n) {
  int i = intern_symbol(n);
  return (scm){ .typ=scm_type_symbol, .val.symbol_id=i };
}

int main(void) {
  init_symbol_table(1);
  init_stack();
  
  stack_push(sym("foo"));
  stack_push(sym("bar"));
  stack_push(sym("foo"));
  stack_push(sym("bar"));
  stack_pop();
  stack_push(sym("baz"));
  stack_push(sym("quux"));
  
  puts(get_symbol(stack_pop().val.symbol_id));
  puts(get_symbol(stack_pop().val.symbol_id));
  puts(get_symbol(stack_pop().val.symbol_id));
  puts(get_symbol(stack_pop().val.symbol_id));
  puts(get_symbol(stack_pop().val.symbol_id));
  stack_pop(); // poping past 0
  
  return EXIT_SUCCESS;
}
