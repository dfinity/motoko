TO-TEST = $(patsubst %.as,_out/%.done,$(wildcard *.as))
# .PHONY: $(TO-TEST)

all:
	../run.sh *.as

accept:
	../run.sh -a *.as

clean:
	rm -rf _out

quick: $(TO-TEST)
#	echo $(TO-TEST)

# run single test, e.g. make AST-56.done
_out/%.done: %.as ../../src/asc ../run.sh
	@ ../run.sh -s $< && touch $@
