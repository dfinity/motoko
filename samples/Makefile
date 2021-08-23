# Makefile for motoko  demos

# This works with bash

MOC_FLAGS=-r -t -v

SAMPLES:=$(basename $(wildcard *.mo))
SUBDIRS:=$(shell ls -d */)

all:
	rm -f *.txt
	@for sample in $(SAMPLES); do \
	  echo "moc $(MOC_FLAGS) $$sample.mo >$$sample.txt 2>&1"; \
	  moc $(MOC_FLAGS) $$sample.mo >$$sample.txt 2>&1; \
	  cat $$sample.txt; \
	done
	@for dir in $(SUBDIRS); do \
	  echo "make -s -C $$dir"; \
	  make -s -C $$dir ; \
	done

clean:
	rm -f *.txt
