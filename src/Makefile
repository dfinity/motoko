# This Makefile is not really a
# dependency tracking makefile, it just
# calls `dune`

MO_IDE_TARGET = _build/default/exes/mo_ide.exe
MO_LD_TARGET = _build/default/exes/mo_ld.exe
MO_DOC_TARGET = _build/default/exes/mo_doc.exe
MOC_TARGET = _build/default/exes/moc.exe
DESER_TARGET = _build/default/exes/deser.exe
CANDID_TESTS_TARGET = _build/default/exes/candid_tests.exe
MOC_JS_TARGET = _build/default/js/moc_js.bc.js
MOC_INTERPRETER_TARGET = _build/default/js/moc_interpreter.bc.js

DUNE_OPTS ?=

NATIVE_TARGETS = $(MO_IDE_TARGET) $(MO_LD_TARGET) $(MO_DOC_TARGET) $(MOC_TARGET) $(DIDC_TARGET) $(DESER_TARGET) $(CANDID_TESTS_TARGET)
JS_TARGETS = $(MOC_JS_TARGET) $(DIDC_JS_TARGET) $(MOC_INTERPRETER_TARGET)
ALL_TARGETS = $(NATIVE_TARGETS) $(JS_TARGETS)

.PHONY: all clean moc mo-doc mo-ide mo-ld moc.js deser candid-tests unit-tests

# let make update check this file every time
SOURCE_ID = source_id/generated.ml

# This targets is spelled out so that `make`
# only invokes `dune` once, much faster
all: exes grammar

exes: $(SOURCE_ID)
	dune build $(DUNE_OPTS) $(NATIVE_TARGETS)
	@ln -fs $(MOC_TARGET) moc
	@ln -fs $(MO_IDE_TARGET) mo-ide
	@ln -fs $(MO_DOC_TARGET) mo-doc
	@ln -fs $(MO_LD_TARGET) mo-ld
	@ln -fs $(DESER_TARGET) deser
	@ln -fs $(CANDID_TESTS_TARGET) candid-tests

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
	ls -al $(MOC_JS_TARGET)

moc_interpreter.js: $(SOURCE_ID)
	dune build $(DUNE_OPTS) $(MOC_INTERPRETER_TARGET)
	@ln -fs $(MOC_INTERPRETER_TARGET) moc_interpreter.js
	ls -al $(MOC_INTERPRETER_TARGET)

deser: $(SOURCE_ID)
	dune build $(DUNE_OPTS) $(DESER_TARGET)
	@ln -fs $(DESER_TARGET) deser

candid-tests: $(SOURCE_ID)
	dune build $(DUNE_OPTS) $(CANDID_TESTS_TARGET)
	@ln -fs $(CANDID_TESTS_TARGET) candid-tests

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
	rm -f moc mo-ide mo-ld moc.js deser $(SOURCE_ID)
	dune clean

test:
	make
	make -C ../test
	make -C ../samples all

test-quick:
	make
	make -C ../test quick

test-parallel:
	make
	make -C ../test parallel
	make -C ../samples all

grammar: ../doc/modules/language-guide/examples/grammar.txt

../doc/modules/language-guide/examples/grammar.txt: mo_frontend/parser.mly gen-grammar/gen-grammar.sh gen-grammar/grammar.sed
	./gen-grammar/gen-grammar.sh $< > $@.tmp
	mv $@.tmp $@
