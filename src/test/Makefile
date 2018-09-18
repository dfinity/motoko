all:
	$(MAKE) -C tc-only
	$(MAKE) -C tc-fail
	$(MAKE) -C run
	$(MAKE) -C run-dfinity

accept:
	$(MAKE) -C tc-only accept
	$(MAKE) -C tc-fail accept
	$(MAKE) -C run accept
	$(MAKE) -C run-dfinity accept

clean:
	$(MAKE) -C tc-only clean
	$(MAKE) -C tc-fail clean
	$(MAKE) -C run clean
	$(MAKE) -C run-dfinity clean

