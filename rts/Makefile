CLANG ?= clang-9
WASM_CLANG ?= clang-9
WASM_LD ?= wasm-ld-9

#
# We manually list all the .c files of libtommath that we care about.
# (Usually the Wasm linker embedded in asc will complain if something is missing.)
#

TOMMATHFILES=\
   mp_init mp_add mp_sub mp_mul mp_zero mp_cmp \
   mp_set_u32 mp_set_i32 mp_get_i32 mp_get_mag_u32 \
   mp_set_u64 mp_set_i64 mp_get_i64 mp_get_mag_u64 \
   mp_div mp_init_copy mp_neg mp_abs mp_2expt mp_expt_u32 mp_set mp_sqr \
   s_mp_add mp_cmp_mag s_mp_sub mp_grow mp_clamp \
   mp_init_size mp_exch mp_clear mp_copy mp_count_bits mp_mul_2d mp_rshd mp_mul_d mp_div_2d mp_mod_2d \
   s_mp_balance_mul s_mp_toom_mul s_mp_toom_sqr s_mp_karatsuba_sqr s_mp_sqr_fast s_mp_sqr s_mp_karatsuba_mul s_mp_mul_digs_fast s_mp_mul_digs mp_init_multi mp_clear_multi mp_mul_2 mp_div_2 mp_div_3 mp_lshd mp_incr mp_decr mp_add_d mp_sub_d

TOMMATHSRC ?= $(PWD)/../../libtommath

#
# Various libtommath flags, in particuliar telling it to use our own memory
# manager
#

TOMMATH_FLAGS = \
  -DMP_32BIT \
  -DMP_MALLOC=mp_malloc \
  -DMP_REALLOC=mp_realloc \
  -DMP_CALLOC=mp_calloc \
  -DMP_FREE=mp_free \
  -DMP_MEMSET=0 \
  -DMP_FIXED_CUTOFFS \
  -DMP_NO_FILE \

#
# clang flags used both for Wasm compilation and native (i.e. test suite) compilation
#

CLANG_FLAGS = \
   --compile  \
   -fpic \
   -fvisibility=hidden \

#
# clang flags only used during Wasm compilation
#

WASM_FLAGS = \
   --target=wasm32-unknown-unknown-wasm -fvisibility=hidden \
   -fno-builtin -ffreestanding \
   --optimize=3 \

#
# Build targets
#

all: as-rts.wasm test_rts

_build/wasm _build/native:
	mkdir -p $@

#
# Building the libtommath files, Wasm and native
#

TOMMATH_WASM_O=$(patsubst %,_build/wasm/tommath_%.o,$(TOMMATHFILES))

_build/wasm/tommath_%.o: $(TOMMATHSRC)/bn_%.c | _build/wasm
	$(WASM_CLANG) -c $(CLANG_FLAGS) $(WASM_FLAGS) $(TOMMATH_FLAGS) $< --output $@

TOMMATH_NATIVE_O=$(patsubst %,_build/native/tommath_%.o,$(TOMMATHFILES))

_build/native/tommath_%.o: $(TOMMATHSRC)/bn_%.c | _build/native
	$(CLANG) -c $(CLANG_FLAGS) $(TOMMATH_FLAGS) $< --output $@

#
# Our part of the RTS, Wasm and native
#

RTSFILES=rts idl bigint

_build/wasm/%.o: %.c | _build/wasm
	$(WASM_CLANG) -c $(CLANG_FLAGS) $(WASM_FLAGS)  $(TOMMATH_FLAGS) -I$(TOMMATHSRC) --std=c11  $< --output $@

_build/native/%.o: %.c | _build/native
	$(CLANG) -c $(CLANG_FLAGS) $(TOMMATH_FLAGS) -I$(TOMMATHSRC) --std=c11  $< --output $@

RTS_WASM_O=$(patsubst %,_build/wasm/%.o,$(RTSFILES))
RTS_NATIVE_O=$(patsubst %,_build/native/%.o,$(RTSFILES))

#
# The actual RTS, a we ship it with the compiler
#

as-rts.wasm: $(RTS_WASM_O) $(TOMMATH_WASM_O)
	$(WASM_LD) -o $@ \
		--import-memory --shared --no-entry --gc-sections \
		--export=__wasm_call_ctors \
		$+

#
# A simple program to do simple tests of rts.c, using native compilation
#

test_rts: test_rts.c $(RTS_NATIVE_O) $(TOMMATH_NATIVE_O)
	$(CLANG) -o $@ \
		$+
clean:
	rm -rf _build as-rts.wasm test_rts

