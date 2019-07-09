ASC=../src/asc
OUTDIR=_out

MODULES=\
  RxTest \

WASM=\
	Rx \

OUTFILES=$(addsuffix .out, $(MODULES)) $(addsuffix .wasm, $(WASM))

OUTPATHS=$(addprefix $(OUTDIR)/, $(OUTFILES))

.PHONY: default all clean alltests

default: all

alltests: $(OUTDIR) $(OUTPATHS)

all: alltests

clean:
	rm -rf $(OUTDIR)

$(OUTDIR):
	@mkdir $@

RX_ACTORSCRIPT_SRC=\
	src/Rx.as \

$(OUTDIR)/RxTest.out: $(RX_ACTORSCRIPT_SRC) \
	test/RxTest.as | $(OUTDIR)
	$(ASC) -r test/RxTest.as > $@

$(OUTDIR)/Rx.wasm: $(RX_ACTORSCRIPT_SRC) | $(OUTDIR)
	$(ASC) -c -o $@ src/Rx.as
