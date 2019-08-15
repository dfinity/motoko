main: main.c rts.c wasm-rt-impl.c
	AFL_HARDEN=1 afl-gcc -o main main.c rts.c wasm-rt-impl.c

rts.c: ../../rts/as-rts.wasm
	wasm2c ../../rts/as-rts.wasm -o rts.c

clean:
	rm -f test.wasm rts.c rts.h main
