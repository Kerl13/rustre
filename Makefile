BIN = rustre.byte

.PHONY: all compile clean

all: compile

compile:
	ocamlbuild -use-ocamlfind $(BIN)

debug:
	ocamlbuild -use-ocamlfind rustre.d.byte

clean:
	ocamlbuild -clean
