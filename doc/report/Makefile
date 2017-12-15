PDF = rustre.pdf
SRC = rustre.md

PANDOC_OPTS = --standalone \
							--smart \
							--variable geometry:"top=3cm, bottom=4cm, left=3cm, right=3cm"

$(PDF): $(SRC)
	pandoc $(PANDOC_OPTS) -o $@ $<

clean:
	rm -f $(PDF)
