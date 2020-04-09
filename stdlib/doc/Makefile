STDLIB ?= ../src
ADocOfMo=./adoc-of-mo.pl

SHELL=bash


STDLIB_FILES= $(patsubst $(STDLIB)/%.mo,%,$(wildcard $(STDLIB)/*.mo))

DOCS = $(patsubst %,_build/%.adoc,$(STDLIB_FILES))

ADOCS = _build/index.adoc $(DOCS)

all: _out/index.html

adocs: $(ADOCS)

clean:
	rm -rf _out _build

_out:
	@mkdir -p $@

_build:
	@mkdir -p $@

_build/%.adoc: $(STDLIB)/%.mo $(ADocOfMo) | _build
	$(ADocOfMo) $* < $< > $@.tmp
	@echo "// Do not edit; This file was machine-generated" > $@
	@echo "" >> $@
	@echo "" >> $@
	cat $@.tmp >> $@
	rm -f $@.tmp

_build/index.adoc: index.adoc | _build
	cp $< $@

_out/index.html: _build/index.adoc $(DOCS) | _out
	# checking if all files are included
	@diff -u \
	  --label "Present" \
	  <(echo $(STDLIB_FILES)| tr ' ' '\n' |sort) \
	  --label "Included" \
	  <(perl -n -e 'print "$$1\n" if m,include::(.*)\.adoc,' $< | sort) \
	 # ok

	asciidoctor $< -o $@

.PHONY: all clean adocs
