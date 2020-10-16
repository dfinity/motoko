TEST_DIRS = fail idl ld mo-idl perf repl run run-deser run-drun trap

all:
	$(foreach test_dir,$(TEST_DIRS),make -C $(test_dir);)

MAKE_PAR := $(MAKE) --no-print-directory --load-average -j $(shell getconf _NPROCESSORS_ONLN) --keep-going

quick:
	$(MAKE_PAR) -C fail quick
	$(MAKE_PAR) -C idl quick
	$(MAKE_PAR) -C ld quick
	$(MAKE_PAR) -C mo-idl quick
	$(MAKE_PAR) -C repl quick
	$(MAKE_PAR) -C run quick

parallel: quick
	$(MAKE_PAR) -C perf quick
	$(MAKE_PAR) -C run-deser quick
	$(MAKE_PAR) -C run-drun quick
	$(MAKE_PAR) -C trap quick

accept:
	$(foreach test_dir,$(TEST_DIRS),make -C $(test_dir) accept;)

clean:
	$(foreach test_dir,$(TEST_DIRS),make -C $(test_dir) clean;)
