all: overview-slides.html index.html

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


# This takes the Antora nav index and turns it into a self-contained
# asciidoctor file
_index.adoc : modules/language-guide/lang-nav.adoc
	sed \
	  -e 's,^\* xref:\(.*.adoc\)\[.*\],include::modules/language-guide/pages/\1[],' \
	  -e 's,^\** xref:\(.*.adoc\)\[.*\],include::modules/language-guide/pages/\1[leveloffset=+1],' \
		$< > $@

index.html: _index.adoc $(glob modules/language-guide/pages/*) $(glob modules/language-guide/examples/*)
	asciidoctor \
	  -a secanchors \
	  -a toc2 \
	  -a toclevels=3 \
	  -a example=../examples/ \
	  --failure-level WARN \
	  _index.adoc -o $@
