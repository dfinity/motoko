DESER ?= ../../src/deser 

_out/%.done: %.bin | _out
	$(DESER) < $< | diff -u $(patsubst %.bin, ok/%.ok, $<) - > $@
	@ cat $@

all: quick

quick: $(patsubst %.bin, _out/%.done, $(wildcard *.bin))

accept:

# TODO

clean:
	rm -rf _out

include ../*.mk
