#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "scm.h"
#include "symbols.c"
#include "stack.c"
#include "garbage.c"

scm sym(char *n) {
  int i = intern_symbol(n);
  return (scm){ .typ=scm_type_symbol, .val.symbol_id=i };
}

scm num(int n) {
  return (scm){ .typ=scm_type_number, .val.number_value=n };
}

scm bool(int b) {
  return (scm){ .typ=scm_type_boolean, .val.boolean_value=b };
}

scm closure(code_ptr f, int len, scm** env) {
  return (scm){ .typ=scm_type_procedure, .val.closure.code=f , .val.closure.env_size=len, .val.closure.environment = env };
}

void lambda436(scm **env) {
  scm k432 = stack_pop();
  assert(env[0]->typ == scm_type_boolean);
  if(env[0]->val.boolean_value) {
    stack_push(k432);
    stack_push(*env[2]);
    stack_push(*env[1]);
  }
  else {
    stack_push(k432);
    stack_push(*env[3]);
    stack_push(*env[1]);
  }
}

void lambda437(scm **env) {
  scm y = stack_pop();
  scm x = stack_pop();
  scm f = stack_pop();
  scm b = stack_pop();
  scm k431 = stack_pop();
  stack_push(k431);
  scm **lambda436441 = gc_alloc(4*sizeof(scm));
  lambda436441[0] = gc_alloc_scm(b);
  lambda436441[1] = gc_alloc_scm(f);
  lambda436441[2] = gc_alloc_scm(x);
  lambda436441[3] = gc_alloc_scm(y);
  stack_push(closure(lambda436,4,lambda436441));
}

void lambda438(scm **env) {
  scm s = stack_pop();
  scm k433 = stack_pop();
  stack_push(k433);
  stack_push(sym("yoo"));
  stack_push(sym("zoo"));
  stack_push(s);
}

void lambda439(scm **env) {
  scm p = stack_pop();
  scm q = stack_pop();
  scm k = stack_pop();
  stack_push(p);
  stack_push(k);
}

void lambda440(scm **env) {
  scm p = stack_pop();
  scm q = stack_pop();
  scm k = stack_pop();
  stack_push(q);
  stack_push(k);
}


void display(scm **env) {
  scm result;
  result = stack_pop();
  if(result.typ == scm_type_number) {
    printf("%d\n", result.val.number_value);
  }
  else if(result.typ == scm_type_symbol) {
    printf("%s\n", get_symbol(result.val.symbol_id));
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
  stack_push(bool(1));
  stack_push(closure(lambda438, 0, NULL));
  stack_push(closure(lambda439, 0, NULL));
  stack_push(closure(lambda440, 0, NULL));
  stack_push(closure(lambda437, 0, NULL));
  
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
