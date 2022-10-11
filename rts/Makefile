SHELL:=bash -O globstar

CLANG ?= clang-14
WASM_CLANG ?= clang-14
WASM_LD ?= wasm-ld-14

#
# We manually list all the .c files of libtommath that we care about.
# (Usually the Wasm linker embedded in moc will complain if something is missing.)
#

TOMMATHFILES = \
   mp_init mp_zero mp_add mp_sub mp_mul mp_cmp \
   mp_set_u32 mp_set_i32 mp_get_i32 mp_get_mag_u32 \
   mp_set_u64 mp_set_i64 mp_get_i64 mp_get_mag_u64 \
   mp_set_double mp_get_double \
   mp_div mp_init_copy mp_neg mp_abs mp_2expt mp_expt_u32 mp_set mp_sqr \
   s_mp_add mp_cmp_mag s_mp_sub mp_grow mp_clamp \
   mp_init_size mp_exch mp_clear mp_copy mp_count_bits mp_mul_2d mp_rshd mp_mul_d mp_div_2d mp_mod_2d \
   s_mp_balance_mul s_mp_toom_mul s_mp_toom_sqr s_mp_karatsuba_sqr s_mp_sqr_fast s_mp_sqr s_mp_karatsuba_mul \
   s_mp_mul_digs_fast s_mp_mul_digs mp_init_multi mp_clear_multi mp_mul_2 mp_div_2 mp_div_3 mp_lshd mp_incr \
   mp_decr mp_add_d mp_sub_d

MUSLFILES = \
  pow pow_data sin cos tan asin acos atan atan2 exp exp_data log log_data fmod \
  floor scalbn frexp strlen strnlen memcpy memset memchr memcmp snprintf vsnprintf vfprintf \
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
  -D__STDC_IEC_559__ \

# Note: the above __STDC_IEC_559__ define is somewhat of a misnomer
#       as only IEEE 754 features are used.

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
# clang flags
#

CLANG_FLAGS = \
   --compile \
   -fpic \
   -fvisibility=hidden \
   --std=c11 \
   --target=wasm32-emscripten \
   -fno-builtin -ffreestanding \
   --optimize=s \
   -resource-dir=$(wildcard $(WASM_CLANG_LIB)/lib/clang/*)

#
# Build targets
#

.PHONY: all

all: mo-rts.wasm mo-rts-debug.wasm

_build:
	mkdir -p $@

_build/wasm:
	mkdir -p $@

_build/i686:
	mkdir -p $@

#
# Let make automatically search these directorys (tommath and musl) for .c files
#

vpath %.c $(MUSLSRC)/src/math $(MUSLSRC)/src/stdio $(MUSLSRC)/src/string $(MUSLSRC)/src/ctype $(TOMMATHSRC)


#
# Building the libtommath files
#

TOMMATH_WASM_O=$(TOMMATHFILES:%=_build/wasm/tommath_%.o)
TOMMATH_WASM_A=_build/libtommath.a

TOMMATH_i686_O=$(TOMMATHFILES:%=_build/i686/tommath_%.o)
TOMMATH_i686_A=_build/libtommath_i686.a

_build/wasm/tommath_%.o: bn_%.c | _build/wasm
	$(WASM_CLANG) $(CLANG_FLAGS) $(TOMMATH_FLAGS) $< --output $@

$(TOMMATH_WASM_A): $(TOMMATH_WASM_O)
	llvm-ar rcs $@ $^
	llvm-ranlib $@

_build/i686/tommath_%.o: bn_%.c | _build/i686
	$(WASM_CLANG) $(CLANG_FLAGS) $(TOMMATH_FLAGS) --target=i686-unknown-linux $< --output $@

$(TOMMATH_i686_A): $(TOMMATH_i686_O)
	llvm-ar rcs $@ $^
	llvm-ranlib $@

#
# Building the musl files
#

MUSL_WASM_O=$(MUSLFILES:%=_build/wasm/musl_%.o)
MUSL_WASM_A=_build/libmusl.a

_build/wasm/musl_%.o: %.c | _build/wasm
	$(WASM_CLANG) $(CLANG_FLAGS) $(MUSL_FLAGS) $< --output $@


$(MUSL_WASM_A): $(MUSL_WASM_O)
	llvm-ar rcs $@ $^
	llvm-ranlib $@

#
# The rust code code of the RTS
#

RTS_RUST_WASM_A=_build/wasm/libmotoko_rts.a
RTS_RUST_DEBUG_WASM_A=_build/wasm/libmotoko_rts_debug.a

# This relies on bash and globstar, see https://stackoverflow.com/questions/2483182/recursive-wildcards-in-gnu-make
RTS_RUST_FILES=$(shell ls **/*.rs)
RTS_CARGO_FILES=$(shell ls **/Cargo.toml)

TOMMATH_BINDINGS_RS=_build/tommath_bindings.rs

$(TOMMATH_BINDINGS_RS): | _build
	bindgen $(TOMMATHSRC)/tommath.h \
	    -o $@ \
	    --use-core --ctypes-prefix=libc --no-layout-tests \
	    --whitelist-function mp_init \
	    --whitelist-function mp_init_copy \
	    --whitelist-function mp_set_u32 \
	    --whitelist-function mp_set_i32 \
	    --whitelist-function mp_get_i32 \
	    --whitelist-function mp_set_u64 \
	    --whitelist-function mp_set_i64 \
	    --whitelist-function mp_get_i64 \
	    --whitelist-function mp_set_double \
	    --whitelist-function mp_get_double \
	    --whitelist-function mp_count_bits \
	    --whitelist-function mp_cmp \
	    --whitelist-function mp_add \
	    --whitelist-function mp_sub \
	    --whitelist-function mp_mul \
	    --whitelist-function mp_div \
	    --whitelist-function mp_div_2d \
	    --whitelist-function mp_neg \
	    --whitelist-function mp_abs \
	    --whitelist-function mp_mul_2d \
	    --whitelist-function mp_expt_u32 \
	    --whitelist-function mp_2expt \
	    --whitelist-function mp_incr \
	    --blacklist-type __int32_t \
	    --blacklist-type __int64_t \
	    --blacklist-type __uint32_t \
	    --blacklist-type __uint64_t \
            -- $(TOMMATH_FLAGS)

	# Whitelist parameters used as libtommath.h has lots of definitions that we don't
	# need. Blacklist parameters are used because bindgen still generates unused type
	# definition with the whitelist parameters.
	#
	# Note that bindgen can't generate Rust macros or functions for CPP macros, so
	# macros like `mp_get_u32` and `mp_isneg` need to be manually implemented.


$(RTS_RUST_WASM_A): $(TOMMATH_BINDINGS_RS) $(RTS_RUST_FILES) $(RTS_CARGO_FILES) | _build/wasm
	cd motoko-rts && cargo build --release --target=wasm32-unknown-emscripten -Zbuild-std=core,alloc
	cp motoko-rts/target/wasm32-unknown-emscripten/release/libmotoko_rts.a $@

$(RTS_RUST_DEBUG_WASM_A): $(TOMMATH_BINDINGS_RS) $(RTS_RUST_FILES) $(RTS_CARGO_FILES) | _build/wasm
	cd motoko-rts && cargo build --target=wasm32-unknown-emscripten -Zbuild-std=core,alloc
	cp motoko-rts/target/wasm32-unknown-emscripten/debug/libmotoko_rts.a $@

#
# The test suite
#

.PHONY: test
test: $(TOMMATH_WASM_A) $(TOMMATH_BINDINGS_RS)
	cd motoko-rts-tests && cargo build --target=wasm32-wasi
	wasmtime --disable-cache motoko-rts-tests/target/wasm32-wasi/debug/motoko-rts-tests.wasm

#
# Putting it all together
#

# These symbols from musl are used by the code generator directly
EXPORTED_SYMBOLS=\
  __wasm_call_ctors \
  memcpy \
  memcmp \
  tan \
  asin \
  acos \
  atan \
  atan2 \
  pow \
  sin \
  cos \
  exp \
  fmod \
  log \

mo-rts.wasm: $(TOMMATH_WASM_A) $(MUSL_WASM_A) $(RTS_RUST_WASM_A)
	$(WASM_LD) -o $@ \
		--import-memory --shared --no-entry --gc-sections \
                $(EXPORTED_SYMBOLS:%=--export=%) \
		--whole-archive \
		$+

mo-rts-debug.wasm: $(RTS_RUST_DEBUG_WASM_A) $(TOMMATH_WASM_A) $(MUSL_WASM_A)
	$(WASM_LD) -o $@ \
		--import-memory --shared --no-entry --gc-sections \
                $(EXPORTED_SYMBOLS:%=--export=%) \
		--whole-archive \
		$+


format:
	cargo fmt --verbose --manifest-path motoko-rts/Cargo.toml
	cargo fmt --verbose --manifest-path motoko-rts-tests/Cargo.toml

clean:
	rm -rf \
	  _build \
	  mo-rts.wasm \
	  mo-rts-debug.wasm \
	  motoko-rts/target \
	  motoko-rts-tests/target \
	  motoko-rts/cargo-home
