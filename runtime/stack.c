
scm *stack;
int stack_count;
int stack_allocated_size;

void init_stack(void) {
  stack_count = 0;
  stack_allocated_size = 64;
  stack = malloc(stack_allocated_size*sizeof(scm));
}

scm *stack_push(scm v) {
  if(stack_count == stack_allocated_size) {
    stack_allocated_size *= 2;
    stack = realloc(stack, stack_allocated_size*sizeof(char*));
  }
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
