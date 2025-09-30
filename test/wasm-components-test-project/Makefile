RUNFLAGS =

SCRIPTS = \
	1_compile_motoko_with_components.sh \
	2_build_motoko_component.sh \
	3_run_motoko_component.sh \
	4_compile_motoko_without_components.sh \
	5_compile_motoko_without_components_flag.sh

all:
	../run.sh $(RUNFLAGS) $(SCRIPTS)

accept:
	../run.sh $(RUNFLAGS) -a $(SCRIPTS)

clean:
	rm -rf _out target

include ../*.mk
