all:
	$(MAKE) -C fail
	$(MAKE) -C run
	$(MAKE) -C run-drun
	$(MAKE) -C repl
	$(MAKE) -C ld
	$(MAKE) -C idl
	$(MAKE) -C as-idl
	$(MAKE) -C trap

MAKE_PAR := $(MAKE) --no-print-directory --load-average -j $(shell getconf _NPROCESSORS_ONLN) --keep-going

quick:
	$(MAKE_PAR) -C fail quick
	$(MAKE_PAR) -C run quick
	$(MAKE_PAR) -C repl quick
	$(MAKE_PAR) -C ld quick
	$(MAKE_PAR) -C idl quick
	$(MAKE_PAR) -C as-idl quick

parallel: quick
	$(MAKE_PAR) -C run-drun quick
	$(MAKE_PAR) -C trap quick

coverage:
	rm -rf _coverage
	mkdir _coverage
	export BISECT_FILE=$$PWD/_coverage/bisect; \
	export SKIP_RUNNING=yes; \
	$(MAKE) -C fail; \
	$(MAKE) -C run; \
	$(MAKE) -C run-drun; \
	$(MAKE) -C repl;
	$(MAKE) -C ld || true
	bisect-ppx-report -I ../src/_build/ -html coverage/ _coverage/bisect*.out
	rm -rf _coverage

accept:
	$(MAKE) -C fail accept
	$(MAKE) -C run accept
	$(MAKE) -C run-drun accept
	$(MAKE) -C repl accept
	$(MAKE) -C ld accept
	$(MAKE) -C idl accept
	$(MAKE) -C as-idl accept
	$(MAKE) -C trap accept
	$(MAKE) -C trap check

clean:
	$(MAKE) -C fail clean
	$(MAKE) -C run clean
	$(MAKE) -C run-drun clean
	$(MAKE) -C repl clean
	$(MAKE) -C ld clean
	$(MAKE) -C as-idl clean
	$(MAKE) -C idl clean
	$(MAKE) -C trap clean

.PHONY: coverage
