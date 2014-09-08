#!/bin/sh
csi parser.scm format-combinators.scm compiler.scm $1
gcc -I runtime/ runtime/main.c -o main
