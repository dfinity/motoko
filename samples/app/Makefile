# Makefile for actorscript  demos

# This works with bash

ASC=../../src/asc
DVM=../../test/dvm.sh
# we need to specify the output if there is more than one .as file
OUT=main.wasm
#ASC_FLAGS=-r -t -v
LIB=list.as types.as server.as client.as

SAMPLES:= test

all:
	$(ASC) --dfinity -o $(OUT) $(LIB) main.as

dvm:
	$(DVM) $(OUT)

clean:
	rm -f $(OUT)
