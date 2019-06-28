# This Makefile is not really a
# dependency tracking makefile, it just
# calls `dune`

AS_IDE_TARGET = _build/default/exes/as_ide.exe
AS_LD_TARGET = _build/default/exes/as_ld.exe
ASC_TARGET = _build/default/exes/asc.exe
DIDC_TARGET = _build/default/exes/didc.exe
ASC_JS_TARGET = _build/default/exes/as_js.bc.js

DUNE_OPTS ?=

ALL_TARGETS = $(AS_IDE_TARGET) $(AS_LD_TARGET) $(ASC_TARGET) $(ASC_JS_TARGET) $(DIDC_TARGET)

.PHONY: all clean asc as-ide as-ld asc.js didc

# This targets is spelled out so that `make`
# only invokes `dune` once, much faster
all:
	dune build $(DUNE_OPTS) $(ALL_TARGETS)
	@ln -fs $(ASC_TARGET) asc
	@ln -fs $(AS_IDE_TARGET) as-ide
	@ln -fs $(AS_LD_TARGET) as-ld
	@ln -fs $(ASC_JS_TARGET) asc.js
	@ln -fs $(DIDC_TARGET) didc

asc:
	dune build $(DUNE_OPTS) $(ASC_TARGET)
	@ln -fs $(ASC_TARGET) asc

as-ide:
	dune build $(DUNE_OPTS) $(AS_IDE_TARGET)
	@ln -fs $(AS_IDE_TARGET) as-ide

as-ld:
	dune build $(DUNE_OPTS) $(AS_LD_TARGET)
	@ln -fs $(AS_LD_TARGET) as-ld

asc.js:
	dune build $(DUNE_OPTS) $(ASC_JS_TARGET)
	@ln -fs $(ASC_JS_TARGET) asc.js

didc:
	dune build $(DUNE_OPTS) $(DIDC_TARGET)
	@ln -fs $(DIDC_TARGET) didc

clean:
	rm -f asc as-ide as-ld asc.js didc
	dune clean

test: $(NAME)
	make -C ../test ASC=$(ASC) all
	make -C ../stdlib ASC=$(ASC) all
	make -C ../samples ASC=$(ASC) all

test-quick: $(NAME)
	make -C ../test ASC=$(ASC) quick
