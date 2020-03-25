# Makefile for motoko  demos

# This works with bash

MOC?=../src/moc
MOC_FLAGS= --package stdlib ../stdlib/src -r -t -v


SAMPLES:=$(basename $(wildcard *.mo))
SUBDIRS:=$(shell ls -d */)

all:
	rm -f *.txt
	@for sample in $(SAMPLES); do \
	  echo "$(MOC) $(MOC_FLAGS) $$sample.mo >$$sample.txt 2>&1"; \
	  $(MOC) $(MOC_FLAGS) $$sample.mo >$$sample.txt 2>&1; \
	  cat $$sample.txt; \
	done
	@for dir in $(SUBDIRS); do \
	  echo "make -s -C $$dir"; \
	  make -s -C $$dir ; \
	done

clean:
	rm -f *.txt
