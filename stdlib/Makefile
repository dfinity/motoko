ASC=../src/asc
OUTDIR=out

## VT100 stuff
HRULE="\x1b[2;34m----------------------------------------------------------------\x1b[0m"
MODULE_NAME="\x1b[1;32mModule:\x1b[1;34m"
BEGIN="\x1b[0;1mBegin...\x1b[0m"
DONE="\x1b[1mDone.\n"$(HRULE)
MODULE_NAME_COLOR="\x1b[0;1;34m"
NO_COLOR="\x1b[0m"

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

.PHONY: default all clean startmsg

default: all

startmsg:
	@echo Begin build: $(MODULE_NAME_COLOR)$(MODULES)$(NO_COLOR)...
	@echo $(HRULE)

all: $(OUTDIR) startmsg $(OUTPATHS)
	@echo Build done : $(MODULE_NAME_COLOR)$(MODULES)$(NO_COLOR)

clean:
	rm -rf $(OUTDIR)

$(OUTDIR):
	@mkdir $(OUTDIR)


$(OUTDIR)/List.out: $(OUTDIR) list.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)

$(OUTDIR)/ListTest.out: $(OUTDIR) list.as listTest.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)

$(OUTDIR)/Trie.out: $(OUTDIR) list.as trie.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)

$(OUTDIR)/Set.out: $(OUTDIR) list.as trie.as set.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)

$(OUTDIR)/SetDb.out: $(OUTDIR) list.as trie.as set.as setDb.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)

$(OUTDIR)/SetDbTest.out: $(OUTDIR) list.as trie.as set.as setDb.as setDbTest.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)

$(OUTDIR)/ProduceExchange.out: $(OUTDIR) list.as trie.as examples/produceExchange.as
	@echo $(MODULE_NAME) $(basename $(notdir $@))
	@echo $(BEGIN)
	$(ASC) -r $(filter-out $(OUTDIR), $^) > $@
	@echo $(DONE)


#########################################################################################
# TODO(Matthew): Figure out why this "compressed" version of the rules doesn't work.

# $(OUTDIR)/List.out: $(OUTDIR) list.as ;	@$(doModule)

# $(OUTDIR)/ListTest.out: $(OUTDIR) list.as listTest.as ; @$(doModule)

# $(OUTDIR)/Trie.out: $(OUTDIR) list.as trie.as ; @$(doModule)

# $(OUTDIR)/Set.out: $(OUTDIR) list.as trie.as set.as ; @$(doModule)

# $(OUTDIR)/SetDb.out: $(OUTDIR) list.as trie.as set.as setDb.as ; @$(doModule)

# $(OUTDIR)/SetDbTest.out: $(OUTDIR) list.as trie.as set.as setDb.as setDbTest.as ; @$(doModule)

# $(OUTDIR)/ProduceExchange.out: $(OUTDIR) list.as trie.as produceExchange.as ; @$(doModule)

# define doModule =
# @echo $(MODULE_NAME) $(basename $(notdir $@))
# @echo $(BEGIN)
# $(ASC) -r $(filter-out $(OUTDIR), $^) > $@
# @echo $(DONE)
# endef
