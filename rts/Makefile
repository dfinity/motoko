SHELL:=bash -O globstar

CLANG ?= clang-10
WASM_CLANG ?= clang-10
WASM_LD ?= wasm-ld-10

#
# We manually list all the .c files of libtommath that we care about.
# (Usually the Wasm linker embedded in moc will complain if something is missing.)
#

TOMMATHFILES = \
   mp_init mp_add mp_sub mp_mul mp_zero mp_cmp \
   mp_set_u32 mp_set_i32 mp_get_i32 mp_get_mag_u32 \
   mp_set_u64 mp_set_i64 mp_get_i64 mp_get_mag_u64 \
   mp_div mp_init_copy mp_neg mp_abs mp_2expt mp_expt_u32 mp_set mp_sqr \
   s_mp_add mp_cmp_mag s_mp_sub mp_grow mp_clamp \
   mp_init_size mp_exch mp_clear mp_copy mp_count_bits mp_mul_2d mp_rshd mp_mul_d mp_div_2d mp_mod_2d \
   s_mp_balance_mul s_mp_toom_mul s_mp_toom_sqr s_mp_karatsuba_sqr s_mp_sqr_fast s_mp_sqr s_mp_karatsuba_mul s_mp_mul_digs_fast s_mp_mul_digs mp_init_multi mp_clear_multi mp_mul_2 mp_div_2 mp_div_3 mp_lshd mp_incr mp_decr mp_add_d mp_sub_d

MUSLFILES = \
  pow pow_data sin cos tan asin acos atan atan2 exp exp_data log log_data fmod \
  towctrans iswspace iswupper iswlower iswalpha wcschr wcslen \
  floor scalbn frexp strnlen memchr snprintf vsnprintf vfprintf \
  __math_oflow __math_uflow __math_xflow __math_divzero __math_invalid \
  __rem_pio2 __rem_pio2_large __sin __cos __tan \
  stubs

TOMMATHSRC ?= $(CURDIR)/../../libtommath
MUSLSRC ?= $(CURDIR)/../../wasi-libc/libc-top-half/musl
MUSL_WASI_SYSROOT ?= $(MUSLSRC)/../../sysroot

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
# Various musl flags, in particular telling it to not have long doubles
# and exclude <errno.h>, which pulls in too many dependencies
#
# Note: we use a bit of magic to get rid of invocations to __fwritex (and similar)
#       - the headers contain a declaration, we rename it to (__fwritex ## __COUNTER__)
#       - similarly the invocation becomes __fwritex_2(...) which we inline immediately
#       Be aware that upon bumps of the musl sources the number of occurrences may jump a bit
#       and will need tweaks/additions below.
#       Similarly we define include guards (to suppress certain headers), but those should be
#       pretty stable.
#       TODO: run `wasm2wat mo-rts.wasm | grep -F '(import' | grep __fwritex_` expecting empty.
#
#       See also https://stackoverflow.com/questions/1597007/creating-c-macro-with-and-line-token-concatenation-with-positioning-macr

MUSL_FLAGS = \
  -isystem $(MUSLSRC)/arch/wasm32 \
  -isystem $(MUSLSRC)/src/include \
  -isystem $(MUSLSRC)/src/internal \
  -isystem $(MUSL_WASI_SYSROOT)/include \
  -I $(MUSLSRC)/../headers/private \
  -I $(MUSLSRC)/src/include \
  -D_ERRNO_H -DEOVERFLOW=75 -DEINVAL=22 \
  -Derrno='(*({ static int bla = 0; &bla; }))' \
  -DNL_ARGMAX=9 \
  -D'TOKENPASTE0(x, y)=x \#\# y' \
  -D'TOKENPASTE(x, y)=TOKENPASTE0(x, y)' \
  -D'__fwritex=TOKENPASTE(__fwritex_,__COUNTER__)' \
  -D'__fwritex_2(s, l, f)=(f->write((f), (s), (l)))' \
  -D'__towrite=TOKENPASTE(__towrite_,__COUNTER__)' \
  -D'__towrite_3(f)=(0)' \
  -D__wasilibc_printscan_no_long_double \
  -D__wasilibc_printscan_full_support_option='""' \
  -D__wasi__ \
  -D__NEED_va_list \
  -D__NEED_off_t \
  -D__NEED_locale_t \
  -Dsqrt=__builtin_sqrt \
  -Dfabs=__builtin_fabs

#
# clang flags used both for Wasm compilation and native (i.e. test suite) compilation
#

CLANG_FLAGS = \
   --compile \
   -fpic \
   -fvisibility=hidden \
   --std=c11

#
# clang flags only used during Wasm compilation
#

WASM_FLAGS = \
   --target=wasm32-emscripten \
   -fno-builtin -ffreestanding \
   --optimize=s \

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

_build/wasm/tommath_%.o: bn_%.c | _build/wasm
	$(WASM_CLANG) $(CLANG_FLAGS) $(WASM_FLAGS) $(TOMMATH_FLAGS) $< --output $@

_build/native/tommath_%.o: bn_%.c | _build/native
	$(CLANG) $(CLANG_FLAGS) $(TOMMATH_FLAGS) $< --output $@

TOMMATH_WASM_O=$(TOMMATHFILES:%=_build/wasm/tommath_%.o)
TOMMATH_NATIVE_O=$(TOMMATHFILES:%=_build/native/tommath_%.o)
MUSL_WASM_O=$(MUSLFILES:%=_build/wasm/musl_%.o)

#
# Look into following directories for source dependencies
#

vpath %.c $(MUSLSRC)/src/math $(MUSLSRC)/src/stdio $(MUSLSRC)/src/string $(MUSLSRC)/src/ctype $(TOMMATHSRC)

#
# Our part of the RTS, Wasm and native
#

RTSFILES=rts idl bigint float char buf utf8_valid closure-table text blob principal

_build/wasm/%.o: %.c rts.h buf.h | _build/wasm
	$(WASM_CLANG) $(CLANG_FLAGS) $(WASM_FLAGS) $(TOMMATH_FLAGS) -I$(TOMMATHSRC) $< --output $@

_build/native/%.o: %.c rts.h buf.h | _build/native
	$(CLANG) $(CLANG_FLAGS) $(TOMMATH_FLAGS) -I$(TOMMATHSRC) $< --output $@

_build/wasm/tommath_%.o: %.c rts.h buf.h | _build/wasm
	$(WASM_CLANG) $(CLANG_FLAGS) $(WASM_FLAGS) $(TOMMATH_FLAGS) $< --output $@

_build/native/tommath_%.o: %.c rts.h buf.h | _build/native
	$(CLANG) $(CLANG_FLAGS) $(TOMMATH_FLAGS) $< --output $@

_build/wasm/musl_%.o: %.c | _build/wasm
	$(WASM_CLANG) $(CLANG_FLAGS) $(WASM_FLAGS) $(MUSL_FLAGS) $< --output $@

.PHONY: _build/wasm/libmotoko_rts.a
_build/wasm/libmotoko_rts.a: | _build/wasm
	# NB. codegen-units=1 is to make debugging easier, not strictly
	# necessary
	cd motoko-rts && \
	    XARGO_HOME=$$PWD/xargo-home CARGO_HOME=$$PWD/cargo-home xargo -v rustc --target=wasm32-unknown-emscripten --release -- \
		-Crelocation-model=pic -Ccodegen-units=1
	cp motoko-rts/target/wasm32-unknown-emscripten/release/libmotoko_rts.a $@

RTS_WASM_O=$(RTSFILES:%=_build/wasm/%.o)
RTS_NATIVE_O=$(RTSFILES:%=_build/native/%.o)
RTS_RUST_WASM_O=_build/wasm/libmotoko_rts.a

#
# The actual RTS, as we ship it with the compiler
#

mo-rts.wasm: $(RTS_RUST_WASM_O) $(RTS_WASM_O) $(TOMMATH_WASM_O) $(MUSL_WASM_O)
	$(WASM_LD) -o $@ \
		--import-memory --shared --no-entry --gc-sections \
		--export=__wasm_call_ctors \
		--whole-archive \
		$+

#
# A simple program to do simple tests of rts.c, using native compilation
#

test_rts: test_rts.c $(RTS_NATIVE_O) $(TOMMATH_NATIVE_O)
	$(CLANG) -o $@ $+

test_leb128: test_leb128.c $(RTS_NATIVE_O) $(TOMMATH_NATIVE_O)
	$(CLANG) -o $@ $+

clean:
	rm -rf _build mo-rts.wasm test_rts test_leb128 motoko-rts/target motoko-rts/xargo-home motoko-rts/cargo-home
