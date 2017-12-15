OCB = ocamlbuild -use-ocamlfind
RUSTRE = ../rustre
PONG_SH = pong.sh
PONG_SRC = ../tests/pong.lus
DISPLAY = pong_display.native
DISPLAY_SRC =$(DISPLAY:.native=.ml)
HEIGHT_LUS = $(shell grep HEIGHT $(PONG_SRC) | head -n 1 | cut -d= -f2)
HEIGHT_DISP = $(shell grep height $(DISPLAY_SRC) | head -n 1 | cut -d= -f2)

.PHONY: all
all: pong display
	@echo '#!/bin/bash' > $(PONG_SH)
	@echo 'DT=0.005' >> $(PONG_SH)
	@echo 'if [ "_$$1" != "_" ]; then DT=$$1; fi' >> $(PONG_SH)
	@echo './timer.sh $$DT | ./pong -silent | ./pong_display.native' >> $(PONG_SH)
	@chmod +x $(PONG_SH)
	@echo "Execute \`./$(PONG_SH) [DT]\` to run pong (example: DT=0.01)"

.PHONY: pong
pong:
	$(RUSTRE) -opt -extract rust -o pong.rs $(PONG_SRC) pong
	rustc -C opt-level=3 pong.rs

.PHONY: display
display:
ifeq ($(HEIGHT_LUS), $(HEIGHT_DISP))
	$(OCB) $(DISPLAY)
else
	$(error Error: heights in $(PONG_SRC) and $(DISPLAY_SRC) do not match)
endif

.PHONY: clean
clean:
	$(OCB) -clean
	rm -f pong
	rm -f *.rs
	rm -f $(PONG_SH)
