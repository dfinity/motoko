# Makefile for actorscript  demos

# This works with bash

ASC?=../../src/asc
DVM?=../../test/dvm.sh
# we need to specify the output if there is more than one .as file
OUT=main.wasm
LIB=list.as types.as server.as client.as

SAMPLES:= test

all:
	rm -f main.txt
	$(ASC) -ancient-system-api -r -t -v main.as >main.txt 2>&1
	cat main.txt

dvm:
	$(ASC) -ancient-system-api -o $(OUT) main.as
	$(DVM) $(OUT)

clean:
	rm -f $(OUT) main.txt
