all:
	$(MAKE) -C fail
	$(MAKE) -C run

diffs:
	$(MAKE) -C fail diffs
	$(MAKE) -C run diffs

accept:
	$(MAKE) -C fail accept
	$(MAKE) -C run accept


all-dfn: all
	$(MAKE) -C run-dfinity

diffs-dfn: diffs
	$(MAKE) -C run-dfinity diffs

accept-dfn: accept
	$(MAKE) -C run-dfinity accept


clean:
	$(MAKE) -C fail clean
	$(MAKE) -C run clean
	$(MAKE) -C run-dfinity clean
