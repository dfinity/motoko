RUNFLAGS = -d

all:
	../run.sh $(RUNFLAGS) *.as

accept:
	../run.sh $(RUNFLAGS) -a *.as

clean:
	rm -rf _out

_out/%.done: RUNFLAGS += -t
include ../*.mk
