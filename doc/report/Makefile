PDF = rustre.pdf
SRC = rustre.md

PANDOC_OPTS = --standalone \
							--variable geometry:"top=2.54cm, bottom=2.54cm, left=2.54cm, right=2.54cm"

$(PDF): $(SRC)
	pandoc $(PANDOC_OPTS) -o $@ $<

clean:
	rm -f $(PDF)
