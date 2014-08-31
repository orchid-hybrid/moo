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

void mul(scm **env) {
  scm x = stack_pop();
  scm cont = stack_pop();
  assert(x.typ == scm_type_number);
  assert(env[0]->typ == scm_type_number);
  stack_push(num(env[0]->val.number_value * x.val.number_value));
  stack_push(cont);
}

void fac(scm **env) {
  scm **mul_env;
  scm n = stack_pop();
  scm cont = stack_pop();
  assert(n.typ == scm_type_number);
  if(n.val.number_value) {
    stack_push(cont);
    mul_env = gc_alloc(1*sizeof(scm));
    mul_env[0] = gc_alloc_scm(n);
    stack_push(closure(mul, 1, mul_env));
    stack_push(num(n.val.number_value - 1));
    stack_push(closure(fac, 0, NULL));
  }
  else {
    stack_push(num(1));
    stack_push(cont);
  }
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
  stack_push(num(7));
  stack_push(closure(fac, 0, NULL));
  
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
