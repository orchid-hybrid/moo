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

int scm_truep(scm *b) {
  assert(b->typ == scm_type_boolean);
  return b->val.boolean_value;
}



void debugger_inspect_spaces(int n) {
  // extremely unsafe
  
  int i;
  
  for(i = 0; i < n; i++) {
    printf("%15d -- %15d\n",
           ((scm*)live_space)[i].typ,
           ((scm*)dead_space)[i].typ);
  }
  puts("");
  puts("");
}



void cons(scm *self) {
  scm **env = self->val.closure.environment;

  scm *b = gc_alloc_scm(stack_pop());
  scm *a = gc_alloc_scm(stack_pop());
  scm cont = stack_pop();
  stack_push((scm){ .typ=scm_type_pair, .val.pair.car=a , .val.pair.cdr=b });
  stack_push(cont);
}

void car(scm *self) {
  scm **env = self->val.closure.environment;

  scm p = stack_pop();
  scm cont = stack_pop();
  assert(p.typ == scm_type_pair);
  stack_push(*p.val.pair.car);
  stack_push(cont);
}

void cdr(scm *self) {
  scm **env = self->val.closure.environment;

  scm p = stack_pop();
  scm cont = stack_pop();
  assert(p.typ == scm_type_pair);
  stack_push(*p.val.pair.cdr);
  stack_push(cont);
}

void null_question(scm *self) {
  scm **env = self->val.closure.environment;

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

// self->val.closure.environment
void display(scm *self) {
  scm **env = self->val.closure.environment;
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

void halt(scm *self) {
  exit(0);
}

#include "../moo.c"

int main(void) {
  scm result;
  
  init_symbol_table(1);
  init_stack();
  init_gc(7);
  
  stack_push(closure(scm_main, 0, NULL));
  
  while(1) {
    sacrifice_children();
    //debugger_inspect_spaces(50);
    result = stack_pop();
    if(result.typ == scm_type_procedure) {
      result.val.closure.code(nursery_hold(result));
    }
    else {
      puts("wrong type");
      break;
    }
  }
  
  return EXIT_SUCCESS;
}
