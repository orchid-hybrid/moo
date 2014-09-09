all:
	csi set.scm parser.scm format-combinators.scm compiler.scm test.scm
	gcc -I runtime/ runtime/main.c -o main

