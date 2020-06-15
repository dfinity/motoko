all:
	$(MAKE) -C fail
	$(MAKE) -C run
	$(MAKE) -C run-drun
	$(MAKE) -C perf
	$(MAKE) -C repl
	$(MAKE) -C ld
	$(MAKE) -C idl
	$(MAKE) -C mo-idl
	$(MAKE) -C trap

MAKE_PAR := $(MAKE) --no-print-directory --load-average -j $(shell getconf _NPROCESSORS_ONLN) --keep-going

quick:
	$(MAKE_PAR) -C fail quick
	$(MAKE_PAR) -C run quick
	$(MAKE_PAR) -C repl quick
	$(MAKE_PAR) -C ld quick
	$(MAKE_PAR) -C idl quick
	$(MAKE_PAR) -C mo-idl quick

parallel: quick
	$(MAKE_PAR) -C run-drun quick
	$(MAKE_PAR) -C perf quick
	$(MAKE_PAR) -C trap quick
	$(MAKE_PAR) -C run-deser quick

accept:
	$(MAKE) -C fail accept
	$(MAKE) -C run accept
	$(MAKE) -C run-drun accept
	$(MAKE) -C perf accept
	$(MAKE) -C repl accept
	$(MAKE) -C ld accept
	$(MAKE) -C idl accept
	$(MAKE) -C mo-idl accept
	$(MAKE) -C trap accept
	$(MAKE) -C run-deser accept

clean:
	$(MAKE) -C fail clean
	$(MAKE) -C run clean
	$(MAKE) -C run-drun clean
	$(MAKE) -C perf clean
	$(MAKE) -C repl clean
	$(MAKE) -C ld clean
	$(MAKE) -C mo-idl clean
	$(MAKE) -C idl clean
	$(MAKE) -C trap clean
