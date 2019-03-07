ASC=../../src/asc
OUTDIR=out
MODULE_NAME="\x1b[1;32mModule:\x1b[1;34m"
BEGIN="\x1b[0;1mBegin...\x1b[0m"
DONE="\x1b[1mDone.\n---------------------------------------------------\x1b[0m"

# Add new module targets here:
MODULES=\
	List \
	ListTest \
	Trie \
	Set \
	SetDb \
	SetDbTest \
	ProduceExchange \

OUTFILES=$(addsuffix .out, $(MODULES))

OUTPATHS=$(addprefix $(OUTDIR)/, $(OUTFILES))

.PHONY: default all clean

default: all

all: $(OUTPATHS)

clean:
	rm -rf $(OUTDIR)

$(OUTDIR):
	mkdir $(OUTDIR)

$(OUTDIR)/List.out: $(OUTDIR) list.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)

$(OUTDIR)/ListTest.out: list.as listTest.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)

$(OUTDIR)/Trie.out: list.as trie.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)

$(OUTDIR)/Set.out: list.as trie.as set.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)

$(OUTDIR)/SetDb.out: list.as trie.as set.as setDb.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)

$(OUTDIR)/SetDbTest.out: list.as trie.as set.as setDb.as setDbTest.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)

$(OUTDIR)/ProduceExchange.out: list.as trie.as produceExchange.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)
