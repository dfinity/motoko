all: rts.wasm

clean:
	rm -f rts.o rts.wasm

rts.o: rts.c
	clang-9 --compile -fpic $< --std=c11 --target=wasm32-unknown-unknown-wasm -fvisibility=hidden --optimize=3 --output $@

rts.wasm: rts.o
	wasm-ld-9 --shared --no-entry $< -o $@ --gc-sections --export=__wasm_call_ctors
