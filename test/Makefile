all:
	$(MAKE) -C fail
	$(MAKE) -C run
	$(MAKE) -C run-dfinity

MAKE_PAR := $(MAKE) --no-print-directory --load-average -j $(shell getconf _NPROCESSORS_ONLN) --keep-going

quick:
	$(MAKE_PAR) -C fail quick
	$(MAKE_PAR) -C run quick

parallel: quick
	$(MAKE_PAR) -C run-dfinity quick

coverage:
	rm -rf _coverage
	mkdir _coverage
	export BISECT_FILE=$$PWD/_coverage/bisect; \
	export SKIP_RUNNING=yes; \
	$(MAKE) -C fail; \
	$(MAKE) -C run; \
	$(MAKE) -C run-dfinity || true
	bisect-ppx-report -I ../src/_build/ -html coverage/ _coverage/bisect*.out
	rm -rf _coverage



accept:
	$(MAKE) -C fail accept
	$(MAKE) -C run accept
	$(MAKE) -C run-dfinity accept

clean:
	$(MAKE) -C fail clean
	$(MAKE) -C run clean
	$(MAKE) -C run-dfinity clean

.PHONY: coverage
