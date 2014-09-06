#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "scm.h"
#include "symbols.c"
#include "stack.c"
#include "garbage.c"

scm num(int n) {
  return (scm){ .typ=scm_type_number, .val.number_value=n };
}

scm closure(code_ptr f, int len, scm** env) {
  return (scm){ .typ=scm_type_procedure, .val.closure.code=f , .val.closure.env_size=len, .val.closure.environment = env };
}

void doubl(scm **env) {
  scm x = stack_pop();
  scm cont = stack_pop();
  assert(x.typ == scm_type_number);
  stack_push(num(2 * x.val.number_value));
  stack_push(cont);
}

void lambda284(scm **env) {
  scm rv282 = stack_pop();
  stack_push(*env[1]);
  stack_push(rv282);
  stack_push(*env[0]);
}

void lambda285(scm **env) {
  scm rv283 = stack_pop();
  scm **lambda284287 = gc_alloc(2*sizeof(scm));
  lambda284287[0] = env[0];
  lambda284287[1] = env[1];
  stack_push(closure(lambda284,2,lambda284287));
  stack_push(rv283);
  stack_push(*env[0]);
}

void lambda286(scm **env) {
  scm x = stack_pop();
  scm f = stack_pop();
  scm k281 = stack_pop();
  scm **lambda285288 = gc_alloc(2*sizeof(scm));
  lambda285288[0] = gc_alloc_scm(f);
  lambda285288[1] = gc_alloc_scm(k281);
  stack_push(closure(lambda285,2,lambda285288));
  stack_push(x);
  stack_push(f);
}

void display(scm **env) {
  scm result;
  result = stack_pop();
  if(result.typ == scm_type_number) {
    printf("%d\n", result.val.number_value);
  }
  else {
    puts("FAIL");
  }
  exit(0);
}

int main(void) {
  scm result;
  
  init_symbol_table(1);
  init_stack();
  init_gc(1);
  
  stack_push(closure(display, 0, NULL));
  stack_push(closure(doubl, 0, NULL));
  stack_push(num(7));
  stack_push(closure(lambda286, 0, NULL));
  
  while(1) {
    result = stack_pop(); // GC issue, how to deal with this
    if(result.typ == scm_type_procedure) {
      result.val.closure.code(result.val.closure.environment);
    }
    else {
      puts("wrong type");
      break;
    }
  }
  
  return EXIT_SUCCESS;
}
