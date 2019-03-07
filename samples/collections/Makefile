ASC=../../src/asc
MODULE_NAME="\x1b[1;32mModule:\x1b[1;34m"
BEGIN="\x1b[0;1mBegin...\x1b[0m"
DONE="\x1b[1mDone.\n---------------------------------------------------\x1b[0m"

.PHONY: default all clean

default: all

all: \
	List.out \
	ListTest.out \
	Trie.out \
	Set.out \
	SetDb.out \
	SetDbTest.out \
	ProduceExchange.out \

clean:
	rm -f *.out

List.out: list.as
	@echo $(MODULE_NAME) $(basename $@)
	@echo $(BEGIN)
	$(ASC) -r $^ > $@
	@echo $(DONE)

ListTest.out: list.as listTest.as
	@echo $(MODULE_NAME) $(basename $@)
	@echo $(BEGIN)
	$(ASC) -r $^ > $@
	@echo $(DONE)

Trie.out: list.as trie.as
	@echo $(MODULE_NAME) $(basename $@)
	@echo $(BEGIN)
	$(ASC) -r $^  > $@
	@echo $(DONE)

Set.out: list.as trie.as set.as
	@echo $(MODULE_NAME) $(basename $@)
	@echo $(BEGIN)
	$(ASC) -r $^ > $@
	@echo $(DONE)

SetDb.out: list.as trie.as set.as setDb.as
	@echo $(MODULE_NAME) $(basename $@)
	@echo $(BEGIN)
	$(ASC) -r $^ > $@
	@echo $(DONE)

SetDbTest.out: list.as trie.as set.as setDb.as setDbTest.as
	@echo $(MODULE_NAME) $(basename $@)
	@echo $(BEGIN)
	$(ASC) -r $^ > $@
	@echo $(DONE)

ProduceExchange.out: list.as trie.as produceExchange.as
	@echo $(MODULE_NAME) $(basename $@)
	@echo $(BEGIN)
	$(ASC) -r $^ > $@
	@echo $(DONE)
