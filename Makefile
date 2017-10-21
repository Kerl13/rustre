BIN = rustre.byte

.PHONY: all compile clean

all: compile

compile:
	ocamlbuild -use-ocamlfind $(BIN)

clean:
	ocamlbuild -clean
