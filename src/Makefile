# This Makefile is not really a
# dependency tracking makefile, it just
# calls `dune`

MO_IDE_TARGET = _build/default/exes/mo_ide.exe
MO_LD_TARGET = _build/default/exes/mo_ld.exe
MO_DOC_TARGET = _build/default/exes/mo_doc.exe
MOC_TARGET = _build/default/exes/moc.exe
DIDC_TARGET = _build/default/exes/didc.exe
MOC_JS_TARGET = _build/default/js/mo_js.bc.js
DIDC_JS_TARGET = _build/default/js/didc_js.bc.js
DESER_TARGET = _build/default/exes/deser.exe

DUNE_OPTS ?=

NATIVE_TARGETS = $(MO_IDE_TARGET) $(MO_LD_TARGET) $(MO_DOC_TARGET) $(MOC_TARGET) $(DIDC_TARGET) $(DESER_TARGET)
JS_TARGETS = $(MOC_JS_TARGET) $(DIDC_JS_TARGET)
ALL_TARGETS = $(NATIVE_TARGETS) $(JS_TARGETS)

.PHONY: all clean moc mo-doc mo-ide mo-ld moc.js didc deser unit-tests didc.js

# let make update check this file every time
SOURCE_ID = source_id/source_id.ml

# This targets is spelled out so that `make`
# only invokes `dune` once, much faster
all: $(SOURCE_ID) grammar
	dune build $(DUNE_OPTS) $(NATIVE_TARGETS)
	@ln -fs $(MOC_TARGET) moc
	@ln -fs $(MO_IDE_TARGET) mo-ide
	@ln -fs $(MO_DOC_TARGET) mo-doc
	@ln -fs $(MO_LD_TARGET) mo-ld
	@ln -fs $(DIDC_TARGET) didc
	@ln -fs $(DESER_TARGET) deser

moc: $(SOURCE_ID)
	dune build $(DUNE_OPTS) $(MOC_TARGET)
	@ln -fs $(MOC_TARGET) moc

mo-ide: $(SOURCE_ID)
	dune build $(DUNE_OPTS) $(MO_IDE_TARGET)
	@ln -fs $(MO_IDE_TARGET) mo-ide

mo-ld: $(SOURCE_ID)
	dune build $(DUNE_OPTS) $(MO_LD_TARGET)
	@ln -fs $(MO_LD_TARGET) mo-ld

mo-doc: $(SOURCE_ID)
	dune build $(DUNE_OPTS) $(MO_DOC_TARGET)
	@ln -fs $(MO_DOC_TARGET) mo-doc

moc.js: $(SOURCE_ID)
	dune build $(DUNE_OPTS) $(MOC_JS_TARGET)
	@ln -fs $(MOC_JS_TARGET) moc.js

didc.js: $(SOURCE_ID)
	dune build $(DUNE_OPTS) $(DIDC_JS_TARGET)
	@ln -fs $(DIDC_JS_TARGET) didc.js

didc: $(SOURCE_ID)
	dune build $(DUNE_OPTS) $(DIDC_TARGET)
	@ln -fs $(DIDC_TARGET) didc

deser: $(SOURCE_ID)
	dune build $(DUNE_OPTS) $(DESER_TARGET)
	@ln -fs $(DESER_TARGET) deser

.PHONY: $(SOURCE_ID)

# only actually touch the file if changed
$(SOURCE_ID):
	source_id/gen.sh

unit-tests: $(SOURCE_ID)
	dune runtest $(DUNE_OPTS)

format:
	ocamlformat --inplace \
	  docs/*.mli \
          docs/*.ml \
          languageServer/*.ml \
          languageServer/*.mli

clean:
	rm -f moc mo-ide mo-ld moc.js didc deser $(SOURCE_ID)
	dune clean

test: $(NAME)
	make -C ../test all
	make -C ../samples all

test-quick: $(NAME)
	make -C ../test quick

grammar: ../doc/modules/language-guide/examples/grammar.txt

../doc/modules/language-guide/examples/grammar.txt: mo_frontend/parser.mly gen-grammar/gen-grammar.sh gen-grammar/grammar.sed
	./gen-grammar/gen-grammar.sh $< > $@.tmp
	mv $@.tmp $@
