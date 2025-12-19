.PHONY: clean test build

clean:
	rm -f *.o
	rm -f *.out
	rm -f test/*.o
	rm -f test/*.so

build:
	cargo build

build_test_lib:
	cd test && gcc -c test_lib.c -o test_lib.o -fPIC
	cd test && gcc -shared test_lib.o -o test_lib.so

test: build build_test_lib
	cd test && . venv/bin/activate && python test_main.py
