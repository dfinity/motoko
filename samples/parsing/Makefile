# Makefile for motoko  demos

# This works with bash

MOC?=../../src/moc
# we need to specify the output if there is more than one .mo file
OUT=lambda-calculus.wasm

SAMPLES:= test

all:
	rm -f lambda-calculus.txt
	$(MOC) --package stdlib $(MOC)/../../stdlib/src -ref-system-api -r -v lambda-calculus.mo >lambda-calculus.txt 2>&1
	cat lambda-calculus.txt

clean:
	rm -f $(OUT) lambda-calculus.txt
