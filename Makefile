.PHONY: clean test build

clean:
	rm -f *.o
	rm -f *.out
	rm -f tmp*

build:
	cargo build

test: build
	cd test && . venv/bin/activate && python test_main.py
