RUNFLAGS =

all:
	../run.sh $(RUNFLAGS) *.wat

accept:
	../run.sh $(RUNFLAGS) -a *.wat

clean:
	rm -rf _out

CLANG?=clang-9
WASM_LD?=wasm-ld-9
AS_LD?=../../src/as-ld

_out/%.lib.o: %.c | _out
	$(CLANG) --compile -fpic --std=c11 --target=wasm32-unknown-unknown-wasm -fvisibility=hidden --optimize=3 \
		-fno-builtin -Wall \
		$< --output $@

_out/%.lib.wasm: _out/%.lib.o | _out
	$(WASM_LD) --import-memory --shared --no-entry --gc-sections \
		--export=__wasm_call_ctors \
		$< -o $@

_out/%.base.wasm: %.wat | _out
	wat2wasm --debug-names --enable-multi-value $< -o $@

_out/%.linked.wasm: _out/%.base.wasm _out/%.lib.wasm
	$(AS_LD) -b _out/$*.base.wasm -l _out/$*.lib.wasm -o _out/$*.linked.wasm

_out/%.wat: _out/%.wasm
	wasm2wat --no-check $< -o $@

_out/%.valid: _out/%.wasm
	wasm-validate $< > $@ 2>&1 || true


include ../*.mk
