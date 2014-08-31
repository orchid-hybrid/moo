char **symbols;
int symbols_count;
int symbols_allocated_size;

void init_symbol_table(int initial_size) {
  symbols_count = 0;
  symbols_allocated_size = initial_size;
  symbols = malloc(symbols_allocated_size*sizeof(char*));
}

int intern_symbol(char *name) {
  int i;
  
  for(i = 0; i < symbols_count; i++) {
    if(!strcmp(name, symbols[i])) {
      return i;
    }
  }
  
  if(symbols_count == symbols_allocated_size) {
    symbols_allocated_size *= 2;
    symbols = realloc(symbols, symbols_allocated_size*sizeof(char*));
  }
  symbols[symbols_count] = name;
  symbols_count++;
  
  return symbols_count-1;
}

char *get_symbol(int i) {
  return symbols[i];
}
