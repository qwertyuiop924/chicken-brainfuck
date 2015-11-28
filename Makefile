all: brainfuck

brainfuck: interpreter.o parser.o tokenizer.o string_helpers.o
	csc -o brainfuck build/interpreter.o build/parser.o build/tokenizer.o \
	  build/string_helpers.o

parser.o: src/parser.scm
	csc -c src/parser.scm -o build/parser.o

interpreter.o: src/interpreter.scm
	csc -c src/interpreter.scm -o build/interpreter.o

tokenizer.o: src/tokenizer.scm
	csc -c src/tokenizer.scm -o build/tokenizer.o

string_helpers.o: src/string_helpers.scm
	csc -c src/string_helpers.scm -o build/string_helpers.o

clean:
	rm build/*
