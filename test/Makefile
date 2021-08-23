QUICK_DIRS = fail idl ld mo-idl repl run
OTHER_DIRS = perf run-deser run-drun trap
TEST_DIRS = $(QUICK_DIRS) $(OTHER_DIRS)

MAKE_PAR := $(MAKE) --no-print-directory --load-average -j $(shell getconf _NPROCESSORS_ONLN) --keep-going

all:
	$(foreach test_dir,$(TEST_DIRS),make -C $(test_dir) &&) true

quick:
	$(foreach test_dir,$(QUICK_DIRS),$(MAKE_PAR) -C $(test_dir) quick &&) true

parallel:
	$(foreach test_dir,$(TEST_DIRS),$(MAKE_PAR) -C $(test_dir) quick &&) true

accept:
	$(foreach test_dir,$(TEST_DIRS),make -C $(test_dir) accept &&) true

clean:
	$(foreach test_dir,$(TEST_DIRS),make -C $(test_dir) clean &&) true
