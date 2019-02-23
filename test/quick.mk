TO-TEST = $(patsubst %.as,_out/%.done,$(wildcard *.as))

quick: $(TO-TEST)

# run single test, e.g. make _out/AST-56.done
_out/%.done: %.as ../../src/asc ../run.sh
	@ mkdir -p _out
	@ (../run.sh $< > $@, && mv $@, $@) || (cat $@,; rm -f $@,; false)
