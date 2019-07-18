CLANG ?= clang-9
WASM_LD ?= wasm-ld-9

TOMMATHFILES=\
   mp_init mp_add mp_sub mp_mul mp_zero mp_cmp \
   mp_set_u32 mp_set_i32 mp_get_i32 mp_get_mag_u32 \
   mp_set_u64 mp_set_i64 mp_get_i64 mp_get_mag_u64 \
   mp_div mp_init_copy mp_neg mp_abs mp_2expt mp_expt_u32 mp_set mp_sqr \
   s_mp_add mp_cmp_mag s_mp_sub mp_grow mp_clamp \
   mp_init_size mp_exch mp_clear mp_copy mp_count_bits mp_mul_2d mp_rshd mp_mul_d mp_div_2d mp_mod_2d \
   s_mp_balance_mul s_mp_toom_mul s_mp_toom_sqr s_mp_karatsuba_sqr s_mp_sqr_fast s_mp_sqr s_mp_karatsuba_mul s_mp_mul_digs_fast s_mp_mul_digs mp_init_multi mp_clear_multi mp_mul_2 mp_div_2 mp_div_3 mp_lshd mp_incr mp_decr mp_add_d mp_sub_d

TOMMATH_O=$(patsubst %,_build/tommath_%.o,$(TOMMATHFILES))

TOMMATHSRC ?= $(PWD)/../../libtommath

MP_FLAGS = \
  -DMP_32BIT \
  -DMP_MALLOC=mp_malloc \
  -DMP_REALLOC=mp_realloc \
  -DMP_CALLOC=mp_calloc \
  -DMP_FREE=mp_free \
  -DMP_MEMSET=0 \
  -DMP_FIXED_CUTOFFS \
  -DMP_NO_FILE \

CLANG_FLAGS = \
   --compile  \
   -fpic \
   --target=wasm32-unknown-unknown-wasm -fvisibility=hidden \
   -fno-builtin -ffreestanding \
   --optimize=3 \
   -fvisibility=hidden \


_build/tommath_%.o: $(TOMMATHSRC)/bn_%.c | _build
	$(CLANG) $(CLANG_FLAGS) $(MP_FLAGS)$< --output $@

all: as-rts.wasm

_build:
	mkdir _build

_build/rts.o: rts.c $(HEADER_STUBS) | _build
	$(CLANG) $(CLANG_FLAGS) $(MP_FLAGS) -I$(TOMMATHSRC) --std=c11  $< --output $@

as-rts.wasm: _build/rts.o $(TOMMATH_O)
	$(WASM_LD) --import-memory --shared --no-entry --gc-sections \
		_build/rts.o $(TOMMATH_O) -o $@ \
		--export=__wasm_call_ctors

clean:
	rm -rf _build as-rts.wasm

