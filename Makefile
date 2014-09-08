all:
	csi parser.scm format-combinators.scm compiler.scm
	gcc -I runtime/ runtime/main.c -o main

