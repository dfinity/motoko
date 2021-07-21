.PHONY: %.only

%.only: %.mo
	../run.sh $(RUNFLAGS) $<
