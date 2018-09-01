# Makefile for actorscript  demos

# This works with bash

ASC=../main.native

SAMPLES:=$(basename $(wildcard *.as))

all:
	rm -f *.txt
	@for sample in $(SAMPLES); do \
	  echo "$(ASC) $$sample.as >$$sample.txt 2>&1"; \
	  $(ASC) $$sample.as >$$sample.txt 2>&1; \
	done

clean:
	rm -f *.txt
