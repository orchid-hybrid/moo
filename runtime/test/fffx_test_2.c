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

/// COMPILER OUTPUT BEGIN

void lambda543(scm **env) {
scm rv541 = stack_pop();
stack_push(*env[1]);
stack_push(rv541);
stack_push(*env[0]);
}

void lambda544(scm **env) {
scm rv542 = stack_pop();
scm** lambda543546 = gc_alloc(2*sizeof(scm));
lambda543546[0] = env[0];
lambda543546[1] = env[1];
stack_push(closure(lambda543, 2, lambda543546));
stack_push(rv542);
stack_push(*env[0]);
}

void lambda545(scm **env) {
scm x = stack_pop();
scm f = stack_pop();
scm k540 = stack_pop();
scm** lambda544547 = gc_alloc(2*sizeof(scm));
lambda544547[0] = gc_alloc_scm(f);
lambda544547[1] = gc_alloc_scm(k540);
stack_push(closure(lambda544, 2, lambda544547));
stack_push(x);
stack_push(f);
}

void scm_main(scm **env) {
  stack_push(closure(display, 0, NULL));
  stack_push(closure(doubl, 0, NULL));
  stack_push(num(7));
  stack_push(closure(lambda545, 0, NULL));
}


/// COMPILER OUTPUT END


int main(void) {
  scm result;
  
  init_symbol_table(1);
  init_stack();
  init_gc(1);
  
  stack_push(closure(scm_main, 0, NULL));
  
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
