MOC=../src/moc
DRUN=drun
OUTDIR=_out
DOCDIR=doc
STOPWATCH_C=./tools/stopwatch.c
MDofMO=./tools/markdown-of-motoko.py
MDofMD=./tools/markdown-of-markdown.py
PANDOC=pandoc
STOPWATCH=$(OUTDIR)/stopwatch

WASM=\
	ActorSpec \
	ActorSpecDemo \
	ProduceExchange \
	Rx \

TESTS=\
	Array \
	ArrayTest \
	BufTest \
	Buf \
	AssocList \
	DocTable \
  Function \
  FunctionTest \
	Hash \
	Hashtbl \
	HashtblTest \
	Int \
	IntTest \
  Iter \
  IterTest \
	List \
	ListTest \
	Nat \
	NatTest \
  None \
  NoneTest \
	Option \
	OptionTest \
	Result \
	Text \
	TextTest \
	Trie \
	TrieSet \
	ProduceExchange \
	producerRemInventory \
	retailerReserveMany \
	evalBulk \
	RxTest \

OUTFILES=$(addsuffix .out, $(TESTS)) $(addsuffix .wasm, $(WASM))

OUTPATHS=$(addprefix $(OUTDIR)/, $(OUTFILES))

.PHONY: default all clean alltests alldoc docMd docHtml profile-wasm profile-interp

default: all

docmsg:
	@echo Begin building documentation in \`$(DOCDIR)\`...
	@echo $(HRULE)

alltests: $(OUTDIR) $(OUTPATHS)

all: alldoc alltests profile-wasm profile-interp

clean:
	rm -rf $(OUTDIR) $(DOCDIR)

$(OUTDIR):
	@mkdir $@

$(OUTDIR)/Array.out: array.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/ArrayTest.out: arrayTest.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/Buf.out: buf.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/BufTest.out: bufTest.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/HashtblTest.out: hashtblTest.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/Hashtbl.out: hashtbl.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/Function.out: function.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/FunctionTest.out: functionTest.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/Int.out: int.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/IntTest.out: intTest.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/Iter.out: iter.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/IterTest.out: iterTest.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/Nat.out: nat.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/NatTest.out: natTest.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/Hash.out: hash.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/None.out: none.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/NoneTest.out: noneTest.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/Option.out: option.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/OptionTest.out: optionTest.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/Result.out: result.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/List.out: list.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/ListTest.out: listTest.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/AssocList.out: assocList.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/Text.out: text.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/TextTest.out: textTest.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@


$(OUTDIR)/Trie.out: trie.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/TrieSet.out: trieSet.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@

$(OUTDIR)/DocTable.out: docTable.mo | $(OUTDIR)
	$(MOC) -r $(filter-out $(OUTDIR), $^) > $@


#########################################################################################################33

##
## Documentation
##

alldoc: docMd docHtml

# Markdown documentation, extracted from the source directory
docMd: \
	$(DOCDIR)/README.md \
	$(DOCDIR)/prelude.md \
	$(DOCDIR)/array.md \
	$(DOCDIR)/buf.md \
	$(DOCDIR)/option.md \
	$(DOCDIR)/result.md \
	$(DOCDIR)/hash.md \
	$(DOCDIR)/list.md \
	$(DOCDIR)/assocList.md \
	$(DOCDIR)/trie.md \
	$(DOCDIR)/trieSet.md \
	$(DOCDIR)/docTable.md \
	$(DOCDIR)/examples/produce-exchange/README.md \
	$(DOCDIR)/examples/produce-exchange/serverTypes.md \
	$(DOCDIR)/examples/produce-exchange/serverLang.md \
	$(DOCDIR)/examples/produce-exchange/serverActor.md \
	$(DOCDIR)/examples/produce-exchange/serverModelTypes.md \
	$(DOCDIR)/examples/produce-exchange/serverModel.md \
	| \
	$(DOCDIR)/ \
	$(DOCDIR)/examples/produce-exchange/

# HTML documentation, extracted from the source directory
docHtml: \
	$(DOCDIR)/README.html \
	$(DOCDIR)/prelude.html \
	$(DOCDIR)/array.html \
	$(DOCDIR)/buf.html \
	$(DOCDIR)/option.html \
	$(DOCDIR)/result.html \
	$(DOCDIR)/hash.html \
	$(DOCDIR)/list.html \
	$(DOCDIR)/assocList.html \
	$(DOCDIR)/trie.html \
	$(DOCDIR)/docTable.html \
	$(DOCDIR)/trieSet.html \
	$(DOCDIR)/examples/produce-exchange/README.html \
	$(DOCDIR)/examples/produce-exchange/serverTypes.html \
	$(DOCDIR)/examples/produce-exchange/serverLang.html \
	$(DOCDIR)/examples/produce-exchange/serverActor.html \
	$(DOCDIR)/examples/produce-exchange/serverModelTypes.html \
	$(DOCDIR)/examples/produce-exchange/serverModel.html \
	| \
	$(DOCDIR)/ \
	$(DOCDIR)/examples/produce-exchange/

$(DOCDIR):
	mkdir $@

$(DOCDIR)/README.md: README.md | $(DOCDIR)
	@echo "<!-- **[ DO NOT EDIT -- GENERATED by a Makefile, on `date` ]** -->" > $@
	@echo "" >> $@
	$(MDofMD) $< >> $@

$(DOCDIR)/examples/produce-exchange/: README.md
	@mkdir -p $@

$(DOCDIR)/examples/produce-exchange/README.md: examples/produce-exchange/README.md | $(DOCDIR)/examples/produce-exchange/
	@echo "<!-- **[ DO NOT EDIT -- GENERATED by a Makefile, on `date` ]** -->" > $@
	@echo "" >> $@
	$(MDofMD) $< >> $@

#########################################################################################################33

##
## PX Unit tests
##

PRODUCE_EXCHANGE_SRC=\
	prelude.mo option.mo hash.mo list.mo assocList.mo trie.mo docTable.mo result.mo \
	examples/produce-exchange/serverTypes.mo \
	examples/produce-exchange/serverLang.mo \
	examples/produce-exchange/serverModelTypes.mo \
	examples/produce-exchange/serverModel.mo \
	examples/produce-exchange/serverActor.mo \

$(OUTDIR)/ProduceExchange.out: $(PRODUCE_EXCHANGE_SRC) \
	examples/produce-exchange/test/simpleSetupAndQuery.mo | $(OUTDIR)
	$(MOC) -stub-system-api -r examples/produce-exchange/test/simpleSetupAndQuery.mo > $@

$(OUTDIR)/retailerReserveMany.out: $(PRODUCE_EXCHANGE_SRC) \
	examples/produce-exchange/test/retailerReserveMany.mo | $(OUTDIR)
	$(MOC) -stub-system-api -r examples/produce-exchange/test/retailerReserveMany.mo > $@

$(OUTDIR)/producerRemInventory.out: $(PRODUCE_EXCHANGE_SRC) \
	examples/produce-exchange/test/producerRemInventory.mo | $(OUTDIR)
	$(MOC) -stub-system-api -r examples/produce-exchange/test/producerRemInventory.mo > $@

$(OUTDIR)/evalBulk.out: $(PRODUCE_EXCHANGE_SRC) \
	examples/produce-exchange/test/evalBulk.mo | $(OUTDIR)
	$(MOC) -stub-system-api -r examples/produce-exchange/test/evalBulk.mo > $@

$(OUTDIR)/ProduceExchange.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -stub-system-api -c -o $@ examples/produce-exchange/serverActor.mo

$(DOCDIR)/%.md: %.mo $(MDofMO) | $(DOCDIR)
	@echo "<!-- **[ Do not edit; This file was machine-generated on `date`. ]** -->" > $@
	@echo "" >> $@
	@echo "" >> $@
	$(MDofMO) $< >> $@

$(DOCDIR)/%.html: $(DOCDIR)/%.md
	$(PANDOC) -f gfm $^ > $@

#########################################################################################################33

##
## ActorSpec Example
##

ACTORSPEC_SRC=\
	array.mo \
	int.mo \
	text.mo \
	examples/actorspec/src/ActorSpec.mo \
	examples/actorspec/demo/Demo.mo \

$(OUTDIR)/ActorSpec.wasm: $(ACTORSPEC_SRC) | $(OUTDIR)
	$(MOC) -c -o $@ examples/actorspec/src/ActorSpec.mo

$(OUTDIR)/ActorSpecDemo.wasm: $(ACTORSPEC_SRC) | $(OUTDIR)
	$(MOC) -c -o $@ examples/actorspec/demo/Demo.mo


#########################################################################################################33

##
## Rx Example
##

RX_SRC=\
	array.mo \
	examples/rx/src/Rx.mo \
	examples/rx/test/RxTest.mo \

$(OUTDIR)/RxTest.out: $(RX_SRC) \
	examples/rx/test/RxTest.mo | $(OUTDIR)
	$(MOC) -r examples/rx/test/RxTest.mo > $@

$(OUTDIR)/Rx.wasm: $(RX_SRC) | $(OUTDIR)
	$(MOC) -c -o $@ examples/rx/src/Rx.mo

#########################################################################################################33

##
## Profiling
##

# to do: Fix my Makefile targets below to make the file shorter; 
#        There is lot of redundancy, but I don't know how to hack the Makefile to overcome it without also making it too complex.

PROFILE_LOAD_SRC=\
	examples/produce-exchange/serverActor.mo \
	examples/produce-exchange/test/profileLoad.mo

PROFILE_LOAD_QUERY_SRC=\
	examples/produce-exchange/serverActor.mo \
	examples/produce-exchange/test/profileLoadQuery.mo

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

$(OUTDIR)/profileFastCounts.csv: examples/produce-exchange/test/profileFastCounts.mo
	$(MOC) -r examples/produce-exchange/test/profileFastCounts.mo > $@

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
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-1.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-02.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-2.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-03.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-3.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-04.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-4.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-05.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-5.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-06.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-6.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-07.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-7.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-08.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-8.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-09.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-9.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-10.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-10.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-20.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-20.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-50.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-50.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

$(OUTDIR)/profileLoadQuery.05-100.csv: $(PRODUCE_EXCHANGE_SRC) $(PROFILE_LOAD_QUERY_SRC) | $(OUTDIR)
	time $(MOC) -r --profile examples/produce-exchange/test/params-5-100.mo $(PROFILE_LOAD_QUERY_SRC) --profile-file $@ $(PROFILE_FIELDS)

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
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-1.mo examples/produce-exchange/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_2.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-2.mo examples/produce-exchange/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_3.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-3.mo examples/produce-exchange/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_4.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-4.mo examples/produce-exchange/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_5.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-5.mo examples/produce-exchange/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_6.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-6.mo examples/produce-exchange/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_7.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-7.mo examples/produce-exchange/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_8.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-8.mo examples/produce-exchange/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_9.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-9.mo examples/produce-exchange/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_10.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-10.mo examples/produce-exchange/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_20.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-20.mo examples/produce-exchange/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_50.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-50.mo examples/produce-exchange/profileActor.mo

$(OUTDIR)/ProduceExchangeLoadQuery_5_100.wasm: $(PRODUCE_EXCHANGE_SRC) | $(OUTDIR)
	$(MOC) -c -no-check-ir -o $@ examples/produce-exchange/test/params-5-100.mo examples/produce-exchange/profileActor.mo


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
