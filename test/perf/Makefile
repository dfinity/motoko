RUNFLAGS = -p

all:
	../run.sh $(RUNFLAGS) *.mo

accept:
	../run.sh $(RUNFLAGS) -a *.mo

clean:
	rm -rf _out cancan.* canister_perf.csv

GC-STRATEGY ?= compacting

cancan.wasm:
	moc \
	  --force-gc --$(GC-STRATEGY)-gc \
	  cancan/CanCan.mo \
	  --package base assetstorage \
	  --package sequence assetstorage \
	-o $@

cancan.drun: cancan.wasm
	( echo create \
	; echo 'install rwlgt-iiaaa-aaaaa-aaaaa-cai $< ""' \
	; echo 'ingress rwlgt-iiaaa-aaaaa-aaaaa-cai createProfile "DIDL\x02n\x01m{\x02q\x00\x05test0\x00"' \
	; echo 'ingress rwlgt-iiaaa-aaaaa-aaaaa-cai createProfile "DIDL\x02n\x01m{\x02q\x00\x05test1\x00"' \
	; echo 'ingress rwlgt-iiaaa-aaaaa-aaaaa-cai createProfile "DIDL\x02n\x01m{\x02q\x00\x05test2\x00"' \
	; for i in {1000..1999} \
	;   do echo 'ingress rwlgt-iiaaa-aaaaa-aaaaa-cai createProfile "DIDL\x02n\x01m{\x02q\x00\x08test'$$i'\x00"' \
	; done \
	) > $@

canister_perf.csv: cancan.drun
	drun $< || (rm -f $@ && false)
	cat $@

include ../*.mk
