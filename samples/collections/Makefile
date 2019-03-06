ASC=../../src/asc
MODULE_NAME="\x1b[1;32mModule:\x1b[1;34m"
BEGIN="\x1b[0;1mBegin...\x1b[0m"
DONE="\x1b[1mDone.\n---------------------------------------------------\x1b[0m"

.PHONY: default all Trie List Set

default: all

all: \
	List \
	Trie \
	Set

List:
	@echo $(MODULE_NAME) $@
	@echo $(BEGIN)
	$(ASC) -r list.as
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
