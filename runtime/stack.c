
scm *stack;
int stack_count;
int stack_allocated_size;

void init_stack(void) {
  stack_count = 0;
  stack_allocated_size = 512;
  stack = malloc(stack_allocated_size*sizeof(scm));
}

scm *stack_push(scm v) {
  /* while (stack_count >= stack_allocated_size) { */
  /*   stack_allocated_size *= 2; */
  /*   stack = realloc(stack, stack_allocated_size*sizeof(char*)); */
  /* } */
  stack[stack_count] = v;
  stack_count++;
  return stack+(stack_count-1);
}

scm stack_pop(void) {
  if(stack_count == 0) {
    return (scm){ .typ=scm_type_null };
  }
  else {
    return stack[--stack_count];
  }
}

scm stack_pop_rest(void) {
  void *memory; 
  scm **cell;
  scm args = (scm){ .typ=scm_type_null};
  scm arg;
  while(1) {
    arg = stack_pop();
    if(arg.typ == scm_stack_marker) {
      stack_push(arg);
      return args;
    } else {
      memory = gc_alloc(2*sizeof(scm*)+2*sizeof(scm));
      cell = memory;
      memory += 2*sizeof(scm*);
      cell[0] = memory;
      memory += sizeof(scm);
      cell[1] = memory;
      *cell[1] = args;
      *cell[0] = arg;
      args = (scm){ .typ=scm_type_pair, .val.cons=cell };
    }
  }
}

