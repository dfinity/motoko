RUNFLAGS = -v
export MOC_UNLOCK_VERIFICATION = viper

all:
	../run.sh $(RUNFLAGS) *.mo

accept:
	../run.sh $(RUNFLAGS) -a *.mo

clean:
	rm -rf _out

include ../*.mk
