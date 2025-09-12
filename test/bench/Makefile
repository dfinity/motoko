RUNFLAGS = -p
export EXTRA_MOC_ARGS = --enhanced-orthogonal-persistence

all:
	../run.sh $(RUNFLAGS) *.mo

accept:
	../run.sh $(RUNFLAGS) -a *.mo

clean:
	rm -rf _out

include ../*.mk
