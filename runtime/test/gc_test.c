#include <stdio.h>
#include <stdlib.h>

#include "scm.h"
#include "symbols.c"
#include "stack.c"
#include "garbage.c"

scm sym(char *n) {
  int i = intern_symbol(n);
  return (scm){ .typ=scm_type_symbol, .val.symbol_id=i };
}

scm nil() {
  return (scm){ .typ=scm_type_null };
}

scm pair(scm *a, scm *b) {
  return (scm){ .typ=scm_type_pair, .val.pair.car=a, .val.pair.cdr=b };
}

void debugger_inspect_spaces(int n) {
  // extremely unsafe
  
  int i;
  
  for(i = 0; i < n; i++) {
    printf("%10d -- %10d\n",
           ((scm*)live_space)[i].typ,
           ((scm*)dead_space)[i].typ);
  }
}

int main(void) {
  scm *s_a, *s_b, *s_c, *s_x, *s_y;
  scm *l1, *l2;
  
  init_symbol_table(1);
  init_stack();
  init_gc(64);
  
  s_a = gc_alloc_scm(sym("a"));
  s_b = gc_alloc_scm(sym("b"));
  s_c = gc_alloc_scm(sym("c"));
  s_x = gc_alloc_scm(sym("x"));
  s_y = gc_alloc_scm(sym("y"));
  
  l1 = gc_alloc_scm(nil());
  l1 = gc_alloc_scm(pair(s_c,l1));
  l1 = gc_alloc_scm(pair(s_b,l1));
  l1 = stack_push(pair(s_a,l1));
  l2 = l1;
  l2 = gc_alloc_scm(pair(s_y,l2));
  l2 = gc_alloc_scm(pair(s_x,l2));
  
  printf("scm_type_null:%d\n", scm_type_null);
  printf("scm_type_pair:%d\n", scm_type_pair);
  printf("scm_type_symbol:%d\n", scm_type_symbol);
  printf("scm_type_procedure:%d\n", scm_type_procedure);
  printf("scm_gc_marked:%d\n", scm_gc_marked);
  
  puts("WELL?");

  debugger_inspect_spaces(10);
  
  gc_garbage_collect();
  
  puts("WELL?");
  debugger_inspect_spaces(10);
  
  return EXIT_SUCCESS;
}
