
test: symbols_test stack_test gc_test
	./symbols_test
	./stack_test
	./gc_test

symbols_test: ../symbols.c symbols_test.c
	gcc -I .. symbols_test.c -o symbols_test

stack_test: ../symbols.c ../stack.c stack_test.c
	gcc -I .. stack_test.c -o stack_test

gc_test: ../symbols.c ../stack.c ../garbage.c gc_test.c
	gcc -I .. gc_test.c -o gc_test

clean:
	rm -f symbols_test
	rm -f stack_test
	rm -f gc_test
