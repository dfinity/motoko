_out/%.done: %.bin | _out
	deser < $< | diff -u $(patsubst %.bin, ok/%.ok, $<) - > $@
	@ cat $@

all: quick

quick: $(patsubst %.bin, _out/%.done, $(wildcard *.bin))

accept: $(wildcard *.bin)
	mkdir -p ok
	rm -f $(wildcard ok/*.ok)
	echo $(foreach FILE,$^,"deser < $(FILE) > $(patsubst %.bin, ok/%.ok, $(FILE)) ;") \
	| sh

clean:
	rm -rf _out

include ../*.mk
