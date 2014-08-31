// internal functions (not intended for outside)
void* gc_alloc_internal(int size, int retry);
scm* gc_traverse_from(int copy, scm *s);

int space_size;
void *alloc_ptr;
void *end_ptr;
void *live_space;
void *dead_space;

void init_gc(int set_size) {
  // set_size is in KB
  space_size = set_size*1024;
  live_space = malloc(space_size);
  dead_space = malloc(space_size);
  alloc_ptr = live_space;
  end_ptr = live_space + space_size;
}

void* gc_alloc_internal(int size, int retry) {
  void *p = alloc_ptr;
  alloc_ptr += size;
  if(alloc_ptr >= end_ptr) {
    if(retry == 1) {
      fprintf(stderr, "FATAL ERROR: GC LOOP");
      exit(-1);
    }
    
    gc_garbage_collect();
    return gc_alloc_internal(size, 1);
  }
  return p;
}

void* gc_alloc(int size) {
  return gc_alloc_internal(size,0);
}

scm* gc_alloc_scm(scm s) {
  scm *new_s = gc_alloc(sizeof(scm));
  *new_s = s;
  return new_s;
}

void gc_garbage_collect(void) {
  int i;
  
  fprintf(stderr, "GARBAGE COLLECTION HAPPENING...");
  
  // swap the spaces around
  alloc_ptr = dead_space;
  dead_space = live_space;
  live_space = alloc_ptr;
  end_ptr = live_space + space_size;
  
  // traverse the graph
  for(i = 0; i < stack_count; i++) {
    gc_traverse_from(0, stack+i);
  }
}

scm* gc_traverse_from(int copy, scm *s) {
  scm *new_s;
  int i;
  
  if(s->typ == scm_gc_marked) {
    return s->val.moved_ptr;
  }
  
  if(copy) {
    new_s = gc_alloc(sizeof(scm));
    *new_s = *s;
    s->typ = scm_gc_marked;
  }
  else {
    new_s = s;
  }
  
  switch(new_s->typ) {
  case scm_type_null:
    break;
  case scm_type_pair:
    new_s->val.pair.car = gc_traverse_from(1, new_s->val.pair.car);
    new_s->val.pair.cdr = gc_traverse_from(1, new_s->val.pair.cdr);
    break;
  case scm_type_symbol:
    break;
  case scm_type_procedure:
    for(i = 0; i < new_s->val.closure.env_size; i++) {
      new_s->val.closure.environment[i] = gc_traverse_from(1, new_s->val.closure.environment[i]);
    }
    break;
  }
  
  return new_s;
}
