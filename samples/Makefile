# Makefile for actorscript  demos

# This works with bash

ASC=../src/asc
ASC_FLAGS=-r -t -v


SAMPLES:=$(basename $(wildcard *.as))
SUBDIRS:=$(shell ls -d */)

all:
	rm -f *.txt
	@for sample in $(SAMPLES); do \
	  echo "$(ASC) $(ASC_FLAGS) $$sample.as >$$sample.txt 2>&1"; \
	  $(ASC) $(ASC_FLAGS) $$sample.as >$$sample.txt 2>&1; \
	done
	@for dir in $(SUBDIRS); do \
	  make -C  $$dir ; \
	done

clean:
	rm -f *.txt
