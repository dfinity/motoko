all: overview-slides.html antora

SRC=modules/language-guide/pages
TMP=modules/language-guide/tmp
OUT=md
SHELL=bash

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

md: base
	rm -r -f $(TMP)
	cp -r $(SRC)/../examples $(OUT)/examples
	cp -r $(SRC) $(TMP)
	./convert.sh $(TMP) $(OUT)

antora:
	rm -rf build
	echo "disabled (nixpkgs 22.05)"
	# antora antora-test-playbook.yml
	# ./patch-antora-js.sh

.PHONY: antora
.PHONY: base
.PHONY: md
