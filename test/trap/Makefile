RUNFLAGS =

all:
	../run.sh $(RUNFLAGS) *.as

accept:
	../run.sh $(RUNFLAGS) -a *.as

clean:
	rm -rf _out

include ../*.mk

.PHONY: check _out/%.checked
check: $(patsubst %.as,_out/%.checked,$(wildcard *.as))

## for each %.as test there should be 4 ok/%.*.ok files with defined contents
_out/%.checked: %.as
	@ (cat $(patsubst %.as,ok/%.run*.ok,$^) \
	| grep -e "execution error, arithmetic overflow" \
	       -e "execution error, numeric overflow" \
	       -e "execution error, value out of bounds" \
	| wc -l | grep 3 > /dev/null) \
	|| (cat $(patsubst %.as,ok/%.run*.ok,$^); ls $(patsubst %.as,ok/%.run*.ok,$^); false)
	@ (cat $(patsubst %.as,ok/%.wasm-run.ok,$^) \
	| grep -e "runtime trap: unreachable" \
	       -e "runtime trap: integer overflow" \
	| wc -l | grep 1 > /dev/null) \
	|| (cat $(patsubst %.as,ok/%.wasm-run.ok,$^); ls $(patsubst %.as,ok/%.wasm-run.ok,$^); false)
	@ (ls $(patsubst %.as,ok/%.*.ok,$^) | wc -l | grep 4 > /dev/null) \
	|| (ls $(patsubst %.as,ok/%.*.ok,$^); false)
