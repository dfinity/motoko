DESER ?= ../../src/deser 

_out/%.done: %.bin | _out
	$(DESER) < $< | diff -u $(patsubst %.bin, ok/%.ok, $<) - > $@
	@ cat $@

all: quick

quick: $(patsubst %.bin, _out/%.done, $(wildcard *.bin))

accept: $(wildcard *.bin)
	rm $(wildcard ok/*.ok)
	echo $(foreach FILE,$^,"$(DESER) < $(FILE) > $(patsubst %.bin, ok/%.ok, $(FILE)) ;") \
	| sh

clean:
	rm -rf _out

include ../*.mk
