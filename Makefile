BIN = rustre
TARGET = byte

.PHONY: all compile clean test

all: compile

compile:
	ocamlbuild -use-ocamlfind $(BIN).$(TARGET)
	mv $(BIN).$(TARGET) $(BIN)

test:
	make -C tests

debug:
	ocamlbuild -use-ocamlfind rustre.d.byte

clean:
	ocamlbuild -clean
	rm -rf $(BIN)
