OCB = ocamlbuild -use-ocamlfind
RUSTRE = ../rustre
PONG_SRC = ../tests/pong.lus
DISPLAY = pong_display.native

.PHONY: all
all: pong display
	@echo "Execute \`./timer.sh | ./pong -silent | ./pong_display.native\` to run pong"

.PHONY: pong
pong:
	$(RUSTRE) -extract rust -o pong.rs $(PONG_SRC) pong
	rustc pong.rs

.PHONY: display
display:
	$(OCB) $(DISPLAY)

.PHONY: clean
clean:
	$(OCB) -clean
	rm -f pong
	rm -f *.rs
