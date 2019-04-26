CLANG = clang-9
WASM_LD = wasm-ld-9

all: rts.wasm

clean:
	rm -f rts.o rts.wasm

rts.o: rts.c
	$(CLANG) --compile -fpic $< --std=c11 --target=wasm32-unknown-unknown-wasm -fvisibility=hidden --optimize=3 --output $@

rts.wasm: rts.o
	$(WASM_LD) --import-memory --shared --no-entry $< -o $@ --gc-sections --export=__wasm_call_ctors
