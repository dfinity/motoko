DESER ?= ../../src/deser

_out/%.done: %.bin $(DESER) | _out
	@ $(DESER) < $< | diff -u $(patsubst %.bin, ok/%.ok, $<) - > $@
	@ cat $@

_out/%.done: %.idv $(DESER) | _out
	@ $(DESER) --reverse < $< | diff -u $(patsubst %.idv, ok/%.ok, $<) - > $@
	@ cat $@

all: quick

quick: $(patsubst %.bin, _out/%.done, $(wildcard *.bin)) $(patsubst %.idv, _out/%.done, $(wildcard *.idv))

accept: $(wildcard *.bin *.idv)
	mkdir -p ok
	rm -f $(wildcard ok/*.ok)
	echo $(foreach FILE,$(filter %.bin,$^),"$(DESER) < $(FILE) > $(patsubst %.bin, ok/%.ok, $(FILE));") \
	     $(foreach FILE,$(filter %.idv,$^),"$(DESER) --reverse < $(FILE) > $(patsubst %.idv, ok/%.ok, $(FILE));") \
	| sh

clean:
	rm -rf _out

include ../*.mk
