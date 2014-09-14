#!/bin/sh
csi set.scm parser.scm format-combinators.scm compiler.scm $1
gcc -fsanitize=address -g -I runtime/ runtime/main.c -o main
