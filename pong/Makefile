OCB = ocamlbuild -use-ocamlfind
DISPLAY = pong_display.native

.PHONY: all
all: display

.PHONY: display
display:
	$(OCB) $(DISPLAY)

.PHONY: clean
clean:
	$(OCB) -clean
	rm -f pong
	rm -f *.rs
