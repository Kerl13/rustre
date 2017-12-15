BIN = rustre
TARGET = byte
FILES = coq_tactic_facto Makefile _tags src/ pong/ tests/ doc/report/rustre.pdf
NAME = baudin_pepin_monat_olivry

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

.PHONY: clean-all
clean-all: clean
	make -C pong/ clean

.PHONY: doc
doc: 
	make -C doc/report

.PHONY: dist
dist: doc clean-all
	@echo "Making dist... Please use only on a fresh, cloned directory"
	mkdir $(NAME)
	cp -r $(FILES) $(NAME)
	tar -cvzf $(NAME).tgz $(NAME)/
	rm -r $(NAME)
	@echo "End of make dist... Please use only on a fresh, cloned directory"
############## Examples ######################################################

.PHONY: pong
pong:
	make -C pong

tests/int_accumulation.mlw:
	./rustre tests/int_accumulation.lus switcher -extract why3 -o tests/int_accumulation.mlw


gen-ocaml:
	cd tests && why3 extract int_accumulation.mlw -o ocaml -D ocaml-unsafe-int
	cd tests/ocaml && ocamlbuild iter.byte
