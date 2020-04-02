CLANG ?= clang-10
WASM_CLANG ?= clang-10
WASM_LD ?= wasm-ld-10

#
# We manually list all the .c files of libtommath that we care about.
# (Usually the Wasm linker embedded in moc will complain if something is missing.)
#

TOMMATHFILES=\
   mp_init mp_add mp_sub mp_mul mp_zero mp_cmp \
   mp_set_u32 mp_set_i32 mp_get_i32 mp_get_mag_u32 \
   mp_set_u64 mp_set_i64 mp_get_i64 mp_get_mag_u64 \
   mp_div mp_init_copy mp_neg mp_abs mp_2expt mp_expt_u32 mp_set mp_sqr \
   s_mp_add mp_cmp_mag s_mp_sub mp_grow mp_clamp \
   mp_init_size mp_exch mp_clear mp_copy mp_count_bits mp_mul_2d mp_rshd mp_mul_d mp_div_2d mp_mod_2d \
   s_mp_balance_mul s_mp_toom_mul s_mp_toom_sqr s_mp_karatsuba_sqr s_mp_sqr_fast s_mp_sqr s_mp_karatsuba_mul s_mp_mul_digs_fast s_mp_mul_digs mp_init_multi mp_clear_multi mp_mul_2 mp_div_2 mp_div_3 mp_lshd mp_incr mp_decr mp_add_d mp_sub_d

MUSLFILES=exp_data pow_data pow fabs __math_invalid __math_oflow __math_uflow __math_xflow

TOMMATHSRC ?= $(shell pwd)/../../libtommath
MUSLSRC ?= $(shell pwd)/../../wasi-libc/libc-top-half/musl

#
# Various libtommath flags, in particular telling it to use our own memory
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
   --target=wasm32-emscripten -fvisibility=hidden \
   -fno-builtin -ffreestanding \
   --optimize=3 \

#
# Build targets
#

.PHONY: all

all: mo-rts.wasm test_rts test_leb128

_build/wasm _build/native:
	mkdir -p $@

#
# Building the libtommath files, Wasm and native
#


_build/wasm/tommath_%.o: $(TOMMATHSRC)/bn_%.c | _build/wasm
	$(WASM_CLANG) $(CLANG_FLAGS) $(WASM_FLAGS) $(TOMMATH_FLAGS) --std=c11 $< --output $@


_build/native/tommath_%.o: $(TOMMATHSRC)/bn_%.c | _build/native
	$(CLANG) $(CLANG_FLAGS) $(TOMMATH_FLAGS) --std=c11 $< --output $@


# make WASM_CC=clang-10 WASM_AR=llvm-ar WASM_NM=llvm-nm WASM_CFLAGS="-O2 -DNDEBUG -isystem $(pwd)/sysroot/include" -j10


TOMMATH_WASM_O=$(TOMMATHFILES:%=_build/wasm/tommath_%.o)
TOMMATH_NATIVE_O=$(TOMMATHFILES:%=_build/native/tommath_%.o)
MUSL_WASM_O=$(MUSLFILES:%=_build/wasm/musl_%.o)

vpath %.c $(MUSLSRC)/src/math

#
# Our part of the RTS, Wasm and native
#

RTSFILES=rts idl bigint buf utf8_valid closure-table text blob url

_build/wasm/_%.o: %.c rts.h buf.h | _build/wasm
	$(WASM_CLANG) $(CLANG_FLAGS) $(WASM_FLAGS) $(TOMMATH_FLAGS) -I$(TOMMATHSRC) --std=c11 $< --output $@

_build/native/_%.o: %.c rts.h buf.h | _build/native
	$(CLANG) $(CLANG_FLAGS) $(TOMMATH_FLAGS) -I$(TOMMATHSRC) --std=c11 $< --output $@

_build/wasm/tommath_%.o: %.c rts.h buf.h | _build/wasm
	$(WASM_CLANG) $(CLANG_FLAGS) $(WASM_FLAGS) $(TOMMATH_FLAGS) -I$(TOMMATHSRC) --std=c11 $< --output $@

_build/native/tommath_%.o: %.c rts.h buf.h | _build/native
	$(CLANG) $(CLANG_FLAGS) $(TOMMATH_FLAGS) -I$(TOMMATHSRC) --std=c11 $< --output $@

_build/wasm/musl_%.o: %.c | _build/wasm
	$(WASM_CLANG) $(CLANG_FLAGS) $(WASM_FLAGS) -I$(MUSLSRC) -isystem $(MUSLSRC)/arch/wasm32 -isystem $(MUSLSRC)/src/include -isystem $(MUSLSRC)/src/internal -isystem $(MUSLSRC)/../../sysroot/include --std=c11 $< --output $@

RTS_WASM_O=$(RTSFILES:%=_build/wasm/_%.o)
RTS_NATIVE_O=$(RTSFILES:%=_build/native/_%.o)

#
# The actual RTS, as we ship it with the compiler
#

mo-rts.wasm: $(RTS_WASM_O) $(TOMMATH_WASM_O) $(MUSL_WASM_O)
	$(WASM_LD) -o $@ \
		--import-memory --shared --no-entry --gc-sections \
		--export=__wasm_call_ctors \
		$+

#
# A simple program to do simple tests of rts.c, using native compilation
#

test_rts: test_rts.c $(RTS_NATIVE_O) $(TOMMATH_NATIVE_O)
	$(CLANG) -o $@ $+

test_leb128: test_leb128.c $(RTS_NATIVE_O) $(TOMMATH_NATIVE_O)
	$(CLANG) -o $@ $+

clean:
	rm -rf _build mo-rts.wasm test_rts test_leb128

