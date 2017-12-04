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
	rm -f *.byte *.native
	rm -f $(BIN)

tests/int_accumulation.mlw:
	./rustre tests/int_accumulation.lus switcher -extract why3 -o tests/int_accumulation.mlw


gen-ocaml:
	cd tests && why3 extract int_accumulation.mlw -o ocaml -D ocaml-unsafe-int
	cd tests/ocaml && ocamlbuild iter.byte
