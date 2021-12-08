RUNFLAGS = -p

all:
	../run.sh $(RUNFLAGS) *.mo

accept:
	../run.sh $(RUNFLAGS) -a *.mo

clean:
	rm -rf _out

cancan.wasm:
	moc \
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
	) > $@

canister_perf.csv: cancan.drun
	drun $< || (rm -f $@ && false)
	cat $@

include ../*.mk
