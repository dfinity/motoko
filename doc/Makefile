all: overview-slides.html html

SHELL=bash
OUT=md

overview-slides.html : overview-slides.md
	pandoc \
	  -t revealjs -s -o overview-slides.html overview-slides.md \
	  -V theme=moon \
	  --strip-comments \
	  -V revealjs-url=https://cdn.jsdelivr.net/npm/reveal.js@4.1.0 \
	  #--standalone \
	  # -V width="\"100%\"" \
	  # -V height="\"100%\"" \
	  # -V margin=0 \
	  # # -V minScale=2 \
	  # # -V maxScale=2

base:
	mo-doc --source $(MOTOKO_BASE) --output $(OUT)/base --format plain

html:
        # TODO: perhaps use node and remark-cli to compile out remark plugins first or use docusaurus to build site properly
	rm -rf html
	mkdir -p html
	for f in md/*.md; do pandoc -f gfm --toc -t html -s --metadata title="$$f" -o html/$$(basename $$f .md).html $$f; done

preview:
	make -C ../src moc_interpreter.js
	cd docusaurus; npm install; npm start


.PHONY: base
.PHONY: md
.PHONY: html
.PHONY: preview
