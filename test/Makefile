all:
	$(MAKE) -C fail
	$(MAKE) -C run
	$(MAKE) -C async

accept:
	$(MAKE) -C fail accept
	$(MAKE) -C run accept
	$(MAKE) -C async accept


all-dfn: all
	$(MAKE) -C run-dfinity all

accept-dfn: accept
	$(MAKE) -C run-dfinity accept


clean:
	$(MAKE) -C fail clean
	$(MAKE) -C run clean
	$(MAKE) -C run-dfinity clean
	$(MAKE) -C asyn clean
