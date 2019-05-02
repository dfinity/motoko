CLANG = clang-9
WASM_LD = wasm-ld-9

TOMMATHFUNCS=\
   mp_init mp_add mp_sub mp_zero mp_cmp mp_init_set_int mp_set_long mp_set_int \
#   mp_mod mp_div mp_init_copy

TOMMATHFILES=\
   $(TOMMATHFUNCS) \
   s_mp_add mp_cmp_mag s_mp_sub mp_grow mp_clamp \
#   mp_init_size mp_exch mp_clear mp_copy mp_count_bits mp_mul_2d mp_lshd mp_rshd mp_mul_d mp_div_2d mp_mod_2d

TOMMATH_O=$(patsubst %,tommath_%.o,$(TOMMATHFILES))

TOMMATHSRC = $(PWD)/../../libtommath

tommath_%.o: $(TOMMATHSRC)/bn_%.c
	$(CLANG) --compile -fpic --target=wasm32-unknown-unknown-wasm -fvisibility=hidden --optimize=3 \
		$< --output $@ \
		-fno-builtin \
		-DMP_MALLOC=mp_malloc -DMP_REALLOC=mp_realloc -DMP_CALLOC=mp_calloc -DMP_FREE=mp_free -DMP_MEMSET=0


all: rts.wasm

rts.o: rts.c
	$(CLANG) --compile -fpic --std=c11 --target=wasm32-unknown-unknown-wasm -fvisibility=hidden --optimize=3 \
		-fno-builtin -Wall \
		-I$(TOMMATHSRC) \
		$< --output $@

rts.wasm: rts.o $(TOMMATH_O)
	$(WASM_LD) --import-memory --shared --no-entry --gc-sections \
		rts.o $(TOMMATH_O) -o $@ \
		--export=__wasm_call_ctors \
		$(patsubst %,--export=%,$(TOMMATHFUNCS))


clean:
	rm -f rts.o $(TOMMATH_O) rts.wasm

