#include <stdio.h>
#include <stdlib.h>

#include "symbols.c"

void s(char *n) {
  printf("%s -> %d\n", n, intern_symbol(n));
}

int main(void) {
  init_symbol_table(1);
  s("foo"); s("bar"); s("baz"); s("quux"); s("baz"); s("moo");
  return EXIT_SUCCESS;
}
