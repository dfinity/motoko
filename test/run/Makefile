all:
	../run.sh *.as

accept:
	../run.sh -a *.as

clean:
	rm -rf _out

TO-TEST = $(patsubst %.as,_out/%.done,$(wildcard *.as))

quick: $(TO-TEST)

# run single test, e.g. make _out/AST-56.done
_out/%.done: %.as ../../src/asc ../run.sh
	@ ../run.sh -s $< && touch $@
