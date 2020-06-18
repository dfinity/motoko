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

ALL_TARGETS = $(MO_IDE_TARGET) $(MO_LD_TARGET) $(MO_DOC_TARGET) $(MOC_TARGET) $(MOC_JS_TARGET) $(DIDC_TARGET) $(DESER_TARGET) ($DIDC_JS_TARGET)

.PHONY: all clean moc mo-doc mo-ide mo-ld moc.js didc deser unit-tests didc.js

# This targets is spelled out so that `make`
# only invokes `dune` once, much faster
all: grammar
	dune build $(DUNE_OPTS) $(ALL_TARGETS)
	@ln -fs $(MOC_TARGET) moc
	@ln -fs $(MO_IDE_TARGET) mo-ide
	@ln -fs $(MO_DOC_TARGET) mo-doc
	@ln -fs $(MO_LD_TARGET) mo-ld
	@ln -fs $(MOC_JS_TARGET) moc.js
	@ln -fs $(DIDC_TARGET) didc
	@ln -fs $(DESER_TARGET) deser

moc:
	dune build $(DUNE_OPTS) $(MOC_TARGET)
	@ln -fs $(MOC_TARGET) moc

mo-ide:
	dune build $(DUNE_OPTS) $(MO_IDE_TARGET)
	@ln -fs $(MO_IDE_TARGET) mo-ide

mo-ld:
	dune build $(DUNE_OPTS) $(MO_LD_TARGET)
	@ln -fs $(MO_LD_TARGET) mo-ld

mo-doc:
	dune build $(DUNE_OPTS) $(MO_DOC_TARGET)
	@ln -fs $(MO_DOC_TARGET) mo-doc

moc.js:
	dune build $(DUNE_OPTS) $(MOC_JS_TARGET)
	@ln -fs $(MOC_JS_TARGET) moc.js

didc.js:
	dune build $(DUNE_OPTS) $(DIDC_JS_TARGET)
	@ln -fs $(DIDC_JS_TARGET) didc.js

didc:
	dune build $(DUNE_OPTS) $(DIDC_TARGET)
	@ln -fs $(DIDC_TARGET) didc

deser:
	dune build $(DUNE_OPTS) $(DESER_TARGET)
	@ln -fs $(DESER_TARGET) deser

unit-tests:
	dune runtest $(DUNE_OPTS)

format:
	ocamlformat --inplace docs/*.mli docs/*.ml languageServer/*.ml languageServer/*.mli

clean:
	rm -f moc mo-ide mo-ld moc.js didc deser
	dune clean

test: $(NAME)
	make -C ../test MOC=$(MOC) all
	make -C ../samples MOC=$(MOC) all

test-quick: $(NAME)
	make -C ../test MOC=$(MOC) quick


grammar: ../doc/modules/language-guide/examples/grammar.txt

../doc/modules/language-guide/examples/grammar.txt: mo_frontend/parser.mly gen-grammar/gen-grammar.sh gen-grammar/grammar.sed
	./gen-grammar/gen-grammar.sh $< > $@.tmp
	mv $@.tmp $@
