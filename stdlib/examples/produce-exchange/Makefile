STDLIB ?= ../../src
MOC ?= ../../../src/moc
DRUN ?= drun

WASMTIME_OPTIONS = --disable-cache --cranelift
OUTDIR=_out

MOC_OPTIONS = --package stdlib $(STDLIB)

TESTS = \
  simpleSetupAndQuery \
  producerRemInventory \
  retailerReserveMany \
  evalBulk \


TESTS_OUT = $(patsubst %, $(OUTDIR)/%.out, $(TESTS))

all: $(TESTS_OUT)

$(OUTDIR)/%.out: test/%.mo src/*.mo | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -r $< >$@

$(OUTDIR):
	@mkdir $@

clean:
	rm -rf $(OUTDIR)


#########################################################################################################

##
## Profiling
##

profile: profile-wasm profile-interp

# to do: Fix my Makefile targets below to make the file shorter; 
#        There is lot of redundancy, but I don't know how to hack the Makefile to overcome it without also making it too complex.

PROFILE_LOAD_SRC=\
	serverActor.mo \
	test/profileLoad.mo

PROFILE_LOAD_QUERY_SRC=\
	serverActor.mo \
	test/profileLoadQuery.mo

PROFILE_FIELDS=\
		--profile-field "hash_bit_length" \
		--profile-field "producer_count" \
		--profile-field "inventory_count" \
		--profile-field "transporter_count" \
		--profile-field "route_count" \
		--profile-field "retailer_join_count" \
		--profile-field "retailer_query_cost" \
		--profile-field "retailer_query_size_max" \

##
## To see the sizes of the workloads below, run `make profile-fast-counts`:
##

profile-fast-counts: \
	$(OUTDIR)/profileFastCounts.csv

$(OUTDIR)/profileFastCounts.csv: test/profileFastCounts.mo
	$(MOC) $(MOC_OPTIONS) -r test/profileFastCounts.mo > $@

profile-interp: \
	$(OUTDIR)/profileLoadQuery.05-01.csv \
	$(OUTDIR)/profileLoadQuery.05-02.csv \
	$(OUTDIR)/profileLoadQuery.05-03.csv \
	$(OUTDIR)/profileLoadQuery.05-04.csv \
	$(OUTDIR)/profileLoadQuery.05-05.csv \
	$(OUTDIR)/profileLoadQuery.05-06.csv \
	$(OUTDIR)/profileLoadQuery.05-07.csv \
	$(OUTDIR)/profileLoadQuery.05-08.csv \
	$(OUTDIR)/profileLoadQuery.05-09.csv \
	$(OUTDIR)/profileLoadQuery.05-10.csv \
	$(OUTDIR)/profileLoadQuery.05-20.csv \
	$(OUTDIR)/profileLoadQuery.05-50.csv \
	$(OUTDIR)/profileLoadQuery.05-100.csv \

## MO wishlist: the following various targets each take a params file that defines two variables used in the common test file.
##              it would be better if we could accept named MO values as CLI params; that feature would obviate the need for the `param-X-Y.mo` files used below:

$(OUTDIR)/profileLoadQuery.05-01.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-1.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-02.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-2.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-03.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-3.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-04.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-4.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-05.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-5.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-06.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-6.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-07.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-7.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-08.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-8.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-09.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-9.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-10.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-10.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-20.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-20.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-50.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-50.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-100.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) $(MOC_OPTIONS) -r --profile test/params-5-100.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

# wasm binaries and time measurements for various-sized PX workloads
profile-wasm: $(OUTDIR) profile-fast-counts \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_1.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_1.wasm.csv \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_2.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_2.wasm.csv \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_3.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_3.wasm.csv \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_4.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_4.wasm.csv \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_5.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_5.wasm.csv \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_6.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_6.wasm.csv \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_7.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_7.wasm.csv \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_8.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_8.wasm.csv \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_9.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_9.wasm.csv \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_10.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_10.wasm.csv \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_20.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_20.wasm.csv \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_50.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_50.wasm.csv \
	\
	$(OUTDIR)/ProduceExchangeLoadQuery_5_100.wasm \
	$(OUTDIR)/ProduceExchangeLoadQuery_5_100.wasm.csv \


$(OUTDIR)/ProduceExchangeLoadQuery_5_1.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-1.mo test/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_2.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-2.mo test/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_3.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-3.mo test/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_4.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-4.mo test/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_5.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-5.mo test/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_6.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-6.mo test/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_7.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-7.mo test/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_8.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-8.mo test/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_9.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-9.mo test/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_10.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-10.mo test/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_20.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-20.mo test/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_50.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-50.mo test/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_100.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) $(MOC_OPTIONS) -c -no-check-ir -o $@ test/params-5-100.mo test/profileActor.mo


$(STOPWATCH): $(STOPWATCH_C)
	$(CC) $< -o $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_1.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_1.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 1, " > $@
	cat $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_2.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_2.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 2, " > $@
	cat $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_3.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_3.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 3, " > $@
	cat $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_4.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_4.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 4, " > $@
	cat $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_5.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_5.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 5, " > $@
	cat $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_6.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_6.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 6, " > $@
	cat $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_7.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_7.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 7, " > $@
	cat $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_8.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_8.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 8, " > $@
	cat $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_9.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_9.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 9, " > $@
	cat $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_10.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_10.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 10, " > $@
	cat $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_20.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_20.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 20, " > $@
	cat $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_50.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_50.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 50, " > $@
	cat $@

$(OUTDIR)/ProduceExchangeLoadQuery_5_100.wasm.csv: $(OUTDIR)/ProduceExchangeLoadQuery_5_100.wasm $(STOPWATCH)
	$(STOPWATCH) "$(DRUN) $< /dev/null" "5, 100, " > $@
	cat $@
