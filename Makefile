BIN = rustre.byte

.PHONY: all compile clean test

all: compile

compile:
	ocamlbuild -use-ocamlfind $(BIN)

test:
	./test.sh $(BIN)

clean:
	ocamlbuild -clean
