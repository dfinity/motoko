STDLIB ?= ../src
MOC ?= ../../src/moc
WASMTIME_OPTIONS = --disable-cache --cranelift

OUTDIR=_out

TESTS = $(wildcard *.mo)

TEST_TARGETS = $(patsubst %.mo,_out/%.checked,$(TESTS))

all: $(TEST_TARGETS) $(OUTDIR)/import_all.checked

STDLIB_FILES= $(wildcard $(STDLIB)/*.mo)

$(OUTDIR):
	@mkdir $@

$(OUTDIR)/import_all.mo: $(STDLIB_FILES) | $(OUTDIR)
	> $@
	for f in $(patsubst $(STDLIB)/%.mo,%,$(STDLIB_FILES)); do \
	  echo "import Import_$$f \"mo:stdlib/$$f\";" >> $@; \
	done

$(OUTDIR)/%.wasm: %.mo | $(OUTDIR)
	$(MOC) -c --package stdlib $(STDLIB) -wasi-system-api -o $@ $<

$(OUTDIR)/%.wasm: $(OUTDIR)/%.mo | $(OUTDIR)
	$(MOC) -c --package stdlib $(STDLIB) -wasi-system-api -o $@ $<

$(OUTDIR)/%.checked: $(OUTDIR)/%.wasm
	wasmtime $(WASMTIME_OPTIONS) $<
	touch $@

clean:
	rm -rf $(OUTDIR)

.PRECIOUS: $(OUTDIR)/%.wasm
