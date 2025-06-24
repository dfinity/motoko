all: overview-slides.html html

RELEASE=0.14.0
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
	mo-doc --source $(MOTOKO_CORE) --package core --output $(OUT)/core --format plain
	mo-doc --source $(MOTOKO_BASE) --package base --output $(OUT)/base --format plain

html:
        # TODO: perhaps use node and remark-cli to compile out remark plugins first or use docusaurus to build site properly
	rm -rf html
	mkdir -p html
	for f in md/*/*.md; do pandoc -f gfm --toc -t html -s --metadata title="$$f" -o html/$$(basename $$f .md).html $$f; done


# for building inside nix using a locally built interpreter
preview:
	make -C ../src moc_interpreter.js
	cp -f ../src/moc_interpreter.js docusaurus/static
	cd docusaurus; npm install; npm run clear; npm start

# for building outside nix using a downloaded interpreter
doc:

	rm -f docusaurus/static/moc_interpreter.js
	wget -O docusaurus/static/moc_interpreter.js https://github.com/dfinity/motoko/releases/download/$(RELEASE)/moc-interpreter-$(RELEASE).js
	cd docusaurus; npm install; npm run clear; npm start

.PHONY: base
.PHONY: md
.PHONY: html
.PHONY: preview
.PHONY: doc

