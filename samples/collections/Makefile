ASC=../../src/asc
MODULE_NAME="\x1b[1;32mModule:\x1b[1;34m"
BEGIN="\x1b[0;1mBegin...\x1b[0m"
DONE="\x1b[1mDone.\n---------------------------------------------------\x1b[0m"

.PHONY: default all Trie List ListTest Set SetDb SetDbTest

default: all

all: \
	List \
	ListTest \
	Trie \
	Set \
	SetDb \
	SetDbTest \

List:
	@echo $(MODULE_NAME) $@
	@echo $(BEGIN)
	$(ASC) -r list.as
	@echo $(DONE)

ListTest:
	@echo $(MODULE_NAME) $@
	@echo $(BEGIN)
	$(ASC) -r list.as listTest.as
	@echo $(DONE)

Trie:
	@echo $(MODULE_NAME) $@
	@echo $(BEGIN)
	$(ASC) -r list.as hashtrie.as
	@echo $(DONE)

Set:
	@echo $(MODULE_NAME) $@
	@echo $(BEGIN)
	$(ASC) -r list.as hashtrie.as set.as
	@echo $(DONE)

SetDb:
	@echo $(MODULE_NAME) $@
	@echo $(BEGIN)
	$(ASC) -r list.as hashtrie.as set.as setDb.as
	@echo $(DONE)

SetDbTest:
	@echo $(MODULE_NAME) $@
	@echo $(BEGIN)
	$(ASC) -r list.as hashtrie.as set.as setDb.as setDbTest.as > $@.out
	@echo $(DONE)
