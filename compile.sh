#!/bin/sh
csi set.scm parser.scm bootstrap-shim.scm format-combinators.scm compiler.scm $1
gcc -I runtime/ runtime/main.c -o main
