enum scm_type {
  scm_type_null,
  scm_type_pair,
  scm_type_symbol,
  scm_type_procedure,
  
  scm_gc_marked,
};

typedef struct scm scm;

struct scm {
  enum scm_type typ;
  union {
    struct {
      struct scm *car;
      struct scm *cdr;
    } pair;
    int symbol_id;
    struct {
      void *code_pointer; // TODO
      int env_size;
      struct scm **environment;
    } closure;
    
    struct scm *moved_ptr;
  } val;
};

void init_symbol_table(int initial_size);
int intern_symbol(char *name);
char *get_symbol(int i);

void init_stack(void);
scm *stack_push(scm v);
scm stack_pop(void);

void init_gc(int set_size);
void* gc_alloc(int size);
scm* gc_alloc_scm(scm s);
void gc_garbage_collect(void);
