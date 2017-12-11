BIN = rustre
TARGET = byte

.PHONY: all
all: compile

.PHONY: compile
compile:
	ocamlbuild -use-ocamlfind $(BIN).$(TARGET)
	mv $(BIN).$(TARGET) $(BIN)

.PHONY: test
test: compile
	make -C tests

.PHONY: debug
debug:
	ocamlbuild -use-ocamlfind rustre.d.byte

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *.byte *.native
	rm -f $(BIN)

############## Examples ######################################################

.PHONY: pong
pong:
	make -C pong

tests/int_accumulation.mlw:
	./rustre tests/int_accumulation.lus switcher -extract why3 -o tests/int_accumulation.mlw


gen-ocaml:
	cd tests && why3 extract int_accumulation.mlw -o ocaml -D ocaml-unsafe-int
	cd tests/ocaml && ocamlbuild iter.byte
