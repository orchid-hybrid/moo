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

int scm_truep(scm b) {
  assert(b.typ == scm_type_boolean);
  return b.val.boolean_value;
}


void cons(scm **env) {
  scm *b = gc_alloc_scm(stack_pop());
  scm *a = gc_alloc_scm(stack_pop());
  scm cont = stack_pop();
  stack_push((scm){ .typ=scm_type_pair, .val.pair.car=a , .val.pair.cdr=b });
  stack_push(cont);
}

void car(scm **env) {
  scm p = stack_pop();
  scm cont = stack_pop();
  assert(p.typ == scm_type_pair);
  stack_push(*p.val.pair.car);
  stack_push(cont);
}

void cdr(scm **env) {
  scm p = stack_pop();
  scm cont = stack_pop();
  assert(p.typ == scm_type_pair);
  stack_push(*p.val.pair.cdr);
  stack_push(cont);
}

void null_question(scm **env) {
  scm p = stack_pop();
  scm cont = stack_pop();
  if(p.typ == scm_type_null) {
    stack_push(bool(1));
  }
  else {
    stack_push(bool(0));
  }
  stack_push(cont);
}



scm closure(code_ptr f, int len, scm** env) {
  return (scm){ .typ=scm_type_procedure, .val.closure.code=f , .val.closure.env_size=len, .val.closure.environment = env };
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
    printf("<type: %d>\n", result.typ);
  }
}

void halt(scm **env) {
  exit(0);
}

#include "../moo.c"

int main(void) {
  scm result;
  
  init_symbol_table(10);
  init_stack();
  init_gc(1);
  
  stack_push(closure(scm_main, 0, NULL));
  
  while(1) {
    sacrifice_children();
    result = stack_pop(); nursery_hold(result);
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
