#include <string.h>

// internal functions (not intended for outside)
void* gc_alloc_internal(int size, int retry);
scm* gc_traverse_from(int copy, scm *s);

int space_size;
void *alloc_ptr;
void *end_ptr;
void *dead_end_ptr;
void *live_space;
void *dead_space;
int nursery_size;
int nursery_index;
scm *nursery; // put things here which you have popped from the stack but aren't garbage

#define GC_IN_SPACE(p) ((void*)(p) >= dead_space && ((void*)(p) < dead_end_ptr))

void init_gc(int set_size) {
  // set_size is in KB
  space_size = set_size*1024;
  live_space = malloc(space_size);
  dead_space = malloc(space_size);
  alloc_ptr = live_space;
  end_ptr = live_space + space_size;
  dead_end_ptr = dead_space + space_size;
  nursery_size = 64*sizeof(scm);
  nursery_index = 0;
  nursery = malloc(nursery_size);
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
  
  fprintf(stdout, "GARBAGE COLLECTION HAPPENING...\n");
  
  // swap the spaces around
  alloc_ptr = dead_space;
  dead_space = live_space;
  live_space = alloc_ptr;
  end_ptr = live_space + space_size;
  dead_end_ptr = dead_space + space_size;
  
  // traverse the graph
  for(i = 0; i < stack_count; i++) {
    gc_traverse_from(0, stack+i);
  }
  
  // traverse the nursery
  for(i = 0; i < nursery_index; i++) {
    gc_traverse_from(0, nursery+i);
  }
  
  memset(dead_space, 0xFE, space_size);
}

scm* gc_traverse_from(int copy, scm *s) {
  scm *new_s;
  scm **tmp;
  int i;
  
  #ifdef DEBUG
  printf("hi type is %d\n", s->typ);
  #endif
  
  if(s->typ == scm_gc_marked) {
    return s->val.moved_ptr;
  }
  
  if(GC_IN_SPACE(s)) {
    new_s = gc_alloc(sizeof(scm));
    *new_s = *s;
    s->typ = scm_gc_marked;
    s->val.moved_ptr = new_s;
  }
  else {
    if(copy) return s; // break infinite loop of heap -> stack -> heap (TODO: is this a real issue?)
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
    //printf("SYM: %s\n", get_symbol(new_s->val.symbol_id));
    break;
  case scm_type_boolean:
    break;
  case scm_type_procedure:
    //puts("A");
    tmp = gc_alloc(new_s->val.closure.env_size*sizeof(scm*));
    for(i = 0; i < new_s->val.closure.env_size; i++) {
      tmp[i] = new_s->val.closure.environment[i];
    }
    new_s->val.closure.environment = tmp;
    //puts("B");
    for(i = 0; i < new_s->val.closure.env_size; i++) {
      //printf("B%d\n",i);
      new_s->val.closure.environment[i] = gc_traverse_from(1, new_s->val.closure.environment[i]);
      #ifdef DEBUG
      if(!new_s->val.closure.environment[i])
        puts("NULLED");
      #endif
    }
    //puts("C");
    break;
  default:
    fprintf(stderr, "GC ERROR: Unimplemented type %d\n", new_s->typ);
    exit(0);
  }
  
  #ifdef DEBUG
  if(!new_s) puts("GIVING YOU A NULL POINTER");
  #endif
  return new_s;
}

scm* nursery_hold(scm s) {
  scm* p;
  p = nursery + nursery_index;
  *p = s;
  nursery_index++;
  if(nursery_index >= nursery_size) {
    fprintf(stderr, "GC error: nursery ran out of space\n");
    exit(0);
  }
  return p;
}

void sacrifice_children(void) {
  nursery_index = 0;
}
