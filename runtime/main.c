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

scm str_alloc(char *s) {
  int len = strlen(s);
  char *s_space = gc_alloc(len);
  strncpy(s_space, s, len);
  return (scm){ .typ=scm_type_string, .val.string_value=s_space };
}

scm num(int n) {
  return (scm){ .typ=scm_type_number, .val.number_value=n };
}

scm make_char(char c) {
  return (scm){ .typ=scm_type_char, .val.char_value=c };
}

scm bool(int b) {
  return (scm){ .typ=scm_type_boolean, .val.boolean_value=b };
}

scm closure(code_ptr f, int len, scm** env) {
  return (scm){ .typ=scm_type_procedure, .val.closure.code=f , .val.closure.env_size=len, .val.closure.environment = env };
}

int scm_truep(scm b) {
  assert(b.typ == scm_type_boolean);
  return b.val.boolean_value;
}

int scm_truepstar(scm *b) {
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


void add(scm *self) {
  scm b = stack_pop();
  scm a = stack_pop();
  scm cont = stack_pop();
  int n = a.val.number_value + b.val.number_value;
  stack_push(num(n));
  stack_push(cont);
}

void sub(scm *self) {
  scm b = stack_pop();
  scm a = stack_pop();
  scm cont = stack_pop();
  int n = a.val.number_value - b.val.number_value;
  stack_push(num(n));
  stack_push(cont);
}

void lt(scm *self) {
  scm b = stack_pop();
  scm a = stack_pop();
  scm cont = stack_pop();
  stack_push(bool(a.val.number_value <  b.val.number_value));
  stack_push(cont);
}

void num_eq(scm *self) {
  scm b = stack_pop();
  scm a = stack_pop();
  scm cont = stack_pop();
  stack_push(bool(a.val.number_value == b.val.number_value));
  stack_push(cont);
}

void cons(scm *self) {
  scm **env = self->val.closure.environment;
  scm *b = gc_alloc_scm(stack_pop());
  scm *a = gc_alloc_scm(stack_pop());
  scm **cell = gc_alloc(2*sizeof(scm*));
  cell[0] = a;
  cell[1] = b;
  scm cont = stack_pop();
  stack_push((scm){ .typ=scm_type_pair, .val.cons=cell });
  stack_push(cont);
}

void car(scm *self) {
  scm **env = self->val.closure.environment;
  scm p = stack_pop();
  scm cont = stack_pop();
  assert(p.typ == scm_type_pair);
  stack_push(*p.val.cons[0]);
  stack_push(cont);
}

void set_car(scm *self) {
  scm **env = self->val.closure.environment;
  scm *value = gc_alloc_scm(stack_pop());
  scm cell = stack_pop();
  scm cont = stack_pop();
  assert(cell.typ == scm_type_pair);
  cell.val.cons[0] = value;
  stack_push((scm){ .typ=scm_type_null});
  stack_push(cont);
}


void cdr(scm *self) {
  scm **env = self->val.closure.environment;

  scm p = stack_pop();
  scm cont = stack_pop();
  assert(p.typ == scm_type_pair);
  stack_push(*p.val.cons[1]);
  stack_push(cont);
}



void generic_type_question(int typ, scm *self) {
  scm **env = self->val.closure.environment;
  
  scm p = stack_pop();
  scm cont = stack_pop();
  if(p.typ == typ) {
    stack_push(bool(1));
  }
  else {
    stack_push(bool(0));
  }
  stack_push(cont);
}

void null_question(scm *self) {
  generic_type_question(scm_type_null, self);
}

void pair_question(scm *self) {
  generic_type_question(scm_type_pair, self);
}

void char_question(scm *self) {
  generic_type_question(scm_type_char, self);
}

void string_question(scm *self) {
  generic_type_question(scm_type_string, self);
}

void boolean_question(scm *self) {
  generic_type_question(scm_type_boolean, self);
}

void procedure_question(scm *self) {
  generic_type_question(scm_type_procedure, self);
}

void number_question(scm *self) {
  generic_type_question(scm_type_number, self);
}

void symbol_question(scm *self) {
  generic_type_question(scm_type_symbol, self);
}

void eq_question(scm *self) {
  scm **env = self->val.closure.environment;
  
  scm v1 = stack_pop();
  scm v2 = stack_pop();
  
  scm cont = stack_pop();
  if(0) { // TODO IMPLEMENT THIS
    stack_push(bool(1));
  }
  else {
    stack_push(bool(0));
  }
  stack_push(cont);

}



// self->val.closure.environment
void putstring(scm *self) {
  scm **env = self->val.closure.environment;
  scm result;
  result = stack_pop();
  if(result.typ == scm_type_number) {
    printf("%lld", result.val.number_value);
  }
  else if(result.typ == scm_type_symbol) {
    printf("%s", get_symbol(result.val.symbol_id));
  }
  else if(result.typ == scm_type_string) {
    printf("%s", result.val.string_value);
  }
  else {
    puts("FAIL");
    printf("<type: %d>\n", result.typ);
  }
}


void char_to_string(scm *self) {
  char s[2];
  
  scm **env = self->val.closure.environment;
  
  scm c = stack_pop();
  scm cont = stack_pop();
  assert(c.typ == scm_type_char);
  s[0] = c.val.char_value;
  s[1] = '\0';
  stack_push(str_alloc(s));
  stack_push(cont);
}

void number_to_string(scm *self) {
  char s[1024];
  
  scm **env = self->val.closure.environment;
  
  scm n = stack_pop();
  scm cont = stack_pop();
  assert(n.typ == scm_type_number);
  sprintf(s, "%lld", n.val.number_value);
  stack_push(str_alloc(s));
  stack_push(cont);
}

void symbol_to_string(scm *self) {
  scm **env = self->val.closure.environment;
  
  scm n = stack_pop();
  scm cont = stack_pop();
  assert(n.typ == scm_type_symbol);
  stack_push(str_alloc(get_symbol(n.val.symbol_id)));
  stack_push(cont);
}

void string_append(scm *self) {
  char *app;
  int s1_len, s2_len;
  scm **env = self->val.closure.environment;
  scm s2 = stack_pop();
  scm s1 = stack_pop();
  scm cont = stack_pop();
  s1_len = strlen(s1.val.string_value);
  s2_len = strlen(s2.val.string_value);
  app = malloc(s1_len+s2_len); // TODO speed this up by building the string manually rather than calling str_alloc
  strncpy(app, s1.val.string_value, s1_len);
  strncpy(app+s1_len, s2.val.string_value, s2_len);
  assert(s1.typ == scm_type_string);
  assert(s2.typ == scm_type_string);
  stack_push(str_alloc(app));
  free(app);
  stack_push(cont);
}

void halt(scm *self) {
  exit(0);
}

#include "../moo.c"

int main(void) {
  scm result;
  
  init_symbol_table(1);
  init_stack();
  init_gc(7000);
  
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
