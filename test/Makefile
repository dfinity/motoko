# TODO: Include ld when it supports Wasm64 
QUICK_DIRS = cmp fail idl mo-idl repl run
OTHER_DIRS = bench perf run-deser run-drun trap
TEST_DIRS = $(QUICK_DIRS) $(OTHER_DIRS)
SOURCE_PATHS ?= ../src # will be overridden when building coverage report with nix

MAKE_PAR := $(MAKE) --no-print-directory --load-average -j $(shell getconf _NPROCESSORS_ONLN) --keep-going

all:
	$(foreach test_dir,$(TEST_DIRS),make -C $(test_dir) &&) true

quick:
	$(foreach test_dir,$(QUICK_DIRS),$(MAKE_PAR) -C $(test_dir) quick &&) true

coverage:
	rm -rf _coverage
	mkdir _coverage
	export BISECT_FILE=$$PWD/_coverage/bisect \
	       SKIP_RUNNING=yes \
	       SKIP_VALIDATE=yes; \
	$(foreach test_dir,$(TEST_DIRS),make -C $(test_dir) &&) true
	bisect-ppx-report html --coverage-path=_coverage $(foreach dir,$(SOURCE_PATHS),--source-path=$(dir) --source-path=$(dir)/_build/default/) -o coverage
	rm -rf _coverage

parallel:
	$(foreach test_dir,$(TEST_DIRS),$(MAKE_PAR) -C $(test_dir) quick &&) true

accept:
	$(foreach test_dir,$(TEST_DIRS),make -C $(test_dir) accept &&) true

clean:
	$(foreach test_dir,$(TEST_DIRS),make -C $(test_dir) clean &&) true
