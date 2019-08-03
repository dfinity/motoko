main: main.c test.c
	AFL_HARDEN=1 afl-gcc -o main main.c test.c wasm-rt-impl.c

test.c: test.wasm
	wasm2c test.wasm -o test.c

test.wasm: ../../src/asc ../../rts/as-rts.wasm test.as
	../../src/asc -no-fake-op -no-multi-value test.as -o test.wasm

clean:
	rm -f test.wasm test.c test.h main
