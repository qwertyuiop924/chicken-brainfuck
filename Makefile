all: clean brainfuck

brainfuck: interpreter.o parser.o tokenizer.o string-helpers.o
	csc -o build/brainfuck build/interpreter.o build/parser.o \
	  build/tokenizer.o build/string-helpers.o
	@echo "Done! Built exectable at: ./build/brainfuck"

parser.o: src/parser.scm
	csc -c src/parser.scm -o build/parser.o

interpreter.o: src/interpreter.scm
	csc -c src/interpreter.scm -o build/interpreter.o

tokenizer.o: src/tokenizer.scm
	csc -c src/tokenizer.scm -o build/tokenizer.o

string-helpers.o: src/string-helpers.scm
	csc -c src/string-helpers.scm -o build/string-helpers.o

clean:
	rm -rf build
	mkdir build
