RUNFLAGS = -t

all:
	../run.sh $(RUNFLAGS) *.mo

accept:
	../run.sh $(RUNFLAGS) -a *.mo

accept-par:
	find . -maxdepth 1 -name '*.mo' -print0 | xargs -0 -P4 -I{} ../run.sh $(RUNFLAGS) -a {}

clean:
	rm -rf _out

include ../*.mk
