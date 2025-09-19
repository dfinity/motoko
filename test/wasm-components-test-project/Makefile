RUNFLAGS =

all:
	../run.sh $(RUNFLAGS) 1_compile_motoko_with_components.sh
	../run.sh $(RUNFLAGS) 2_build_motoko_component.sh
	../run.sh $(RUNFLAGS) 3_run_motoko_component.sh

accept:
	../run.sh $(RUNFLAGS) -a 1_compile_motoko_with_components.sh  2_build_motoko_component.sh  3_run_motoko_component.sh

clean:
	rm -rf _out target

include ../*.mk
