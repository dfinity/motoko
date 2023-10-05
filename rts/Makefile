SHELL:=bash -O globstar

CLANG ?= clang-13
WASM_CLANG ?= clang-13
WASM_LD ?= wasm-ld-13

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
   pow_data exp_data log_data \
   floor scalbn frexp strlen strnlen memcpy memset memchr memcmp \
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
  -DMP_PREC=4 \
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
#       TODO: run `wasm2wat --enable-memory64 mo-rts.wasm | grep -F '(import' | grep __fwritex_` expecting empty.
#
#       See also https://stackoverflow.com/questions/1597007/creating-c-macro-with-and-line-token-concatenation-with-positioning-macr

# libc does not yet have a 64-bit port, using 32-bit version instead.
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
  -Dfabs=__builtin_fabs \
  -mbulk-memory \
  -DBULK_MEMORY_THRESHOLD=32

#
# clang flags
#

CLANG_FLAGS = \
   --compile \
   -fpic \
   -fvisibility=hidden \
   --std=c11 \
   --target=wasm64 \
   -fno-builtin -ffreestanding \
   --optimize=s \
   -resource-dir=$(wildcard $(WASM_CLANG_LIB)/lib/clang/*)

#
# Build targets
#

.PHONY: all

all: mo-rts.wasm mo-rts-debug.wasm mo-rts-incremental.wasm mo-rts-incremental-debug.wasm

_build:
	mkdir -p $@

_build/wasm:
	mkdir -p $@

_build/i686:
	mkdir -p $@

# 
# Common configuration
# 

RTS_EMSCRIPTEN=emcc -c --target=wasm64-unknown-emscripten -fpic
RTS_EMSCRIPTEN_DEBUG=$(RTS_EMSCRIPTEN)
RTS_EMSCRIPTEN_RELEASE=$(RTS_EMSCRIPTEN) -O2

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

_build/wasm/musl_%.bc: %.c | _build/wasm
	$(WASM_CLANG) $(CLANG_FLAGS) $(MUSL_FLAGS) -emit-llvm $< --output $@

_build/wasm/musl_%.o: _build/wasm/musl_%.bc
	$(RTS_EMSCRIPTEN_RELEASE) -o $@ $< 

$(MUSL_WASM_A): $(MUSL_WASM_O)
	llvm-ar rcs $@ $^
	llvm-ranlib $@

#
# The rust code code of the RTS
#

# This relies on bash and globstar, see https://stackoverflow.com/questions/2483182/recursive-wildcards-in-gnu-make
RTS_RUST_FILES=$(shell ls **/*.rs)
RTS_CARGO_FILES=$(shell ls **/Cargo.toml)

TOMMATH_BINDINGS_RS=_build/tommath_bindings.rs

$(TOMMATH_BINDINGS_RS): | _build
	bindgen $(TOMMATHSRC)/tommath.h \
	    -o $@ \
	    --use-core --ctypes-prefix="crate::libc_declarations" --no-layout-tests \
	    --allowlist-function mp_init \
	    --allowlist-function mp_init_copy \
	    --allowlist-function mp_set_u32 \
	    --allowlist-function mp_set_i32 \
	    --allowlist-function mp_get_i32 \
	    --allowlist-function mp_set_u64 \
	    --allowlist-function mp_set_i64 \
	    --allowlist-function mp_get_i64 \
	    --allowlist-function mp_set_double \
	    --allowlist-function mp_get_double \
	    --allowlist-function mp_count_bits \
	    --allowlist-function mp_cmp \
	    --allowlist-function mp_add \
	    --allowlist-function mp_sub \
	    --allowlist-function mp_mul \
	    --allowlist-function mp_div \
	    --allowlist-function mp_div_2d \
	    --allowlist-function mp_neg \
	    --allowlist-function mp_abs \
	    --allowlist-function mp_mul_2d \
	    --allowlist-function mp_expt_u32 \
	    --allowlist-function mp_2expt \
	    --allowlist-function mp_incr \
	    --blocklist-type __int32_t \
	    --blocklist-type __int64_t \
	    --blocklist-type __uint32_t \
	    --blocklist-type __uint64_t \
            -- $(TOMMATH_FLAGS)

	# Whitelist parameters used as libtommath.h has lots of definitions that we don't
	# need. Blacklist parameters are used because bindgen still generates unused type
	# definition with the whitelist parameters.
	#
	# Note that bindgen can't generate Rust macros or functions for CPP macros, so
	# macros like `mp_get_u32` and `mp_isneg` need to be manually implemented.

RTS_DEPENDENCIES=$(TOMMATH_BINDINGS_RS) $(RTS_RUST_FILES) $(RTS_CARGO_FILES) | _build/wasm
COMPILER_FLAGS=--emit=llvm-ir
RTS_BUILD=cd motoko-rts && RUSTFLAGS="${COMPILER_FLAGS}" cargo build --target=wasm64-unknown-unknown -Zbuild-std=core,alloc
RTS_DEBUG_BUILD=$(RTS_BUILD)
RTS_RELEASE_BUILD=$(RTS_BUILD) --release

RTS_DEBUG_TARGET_FOLDER=motoko-rts/target/wasm64-unknown-unknown/debug
RTS_RELEASE_TARGET_FOLDER=motoko-rts/target/wasm64-unknown-unknown/release

RTS_DEBUG_TARGET=$(RTS_DEBUG_TARGET_FOLDER)/deps/motoko_rts-*.ll
RTS_RELEASE_TARGET=$(RTS_RELEASE_TARGET_FOLDER)/deps/motoko_rts-*.ll

CORE_DEBUG_TARGET=$(RTS_DEBUG_TARGET_FOLDER)/deps/core-*.ll
CORE_RELEASE_TARGET=$(RTS_RELEASE_TARGET_FOLDER)/deps/core-*.ll

ALLOC_DEBUG_TARGET=$(RTS_DEBUG_TARGET_FOLDER)/deps/alloc-*.ll
ALLOC_RELEASE_TARGET=$(RTS_RELEASE_TARGET_FOLDER)/deps/alloc-*.ll

COMPILER_BUILTINS_DEBUG_TARGET=$(RTS_DEBUG_TARGET_FOLDER)/deps/compiler_builtins-*.ll
COMPILER_BUILTINS_RELEASE_TARGET=$(RTS_RELEASE_TARGET_FOLDER)/deps/compiler_builtins-*.ll

BUILD_FOLDER=_build/wasm

RTS_RUST_NON_INCREMENTAL_LLVM_IR=$(BUILD_FOLDER)/libmotoko_rts.ll
RTS_RUST_NON_INCREMENTAL_DEBUG_LLVM_IR=$(BUILD_FOLDER)/libmotoko_rts_debug.ll

RTS_RUST_INCREMENTAL_LLVM_IR=$(BUILD_FOLDER)/libmotoko_rts_incremental.ll
RTS_RUST_INCREMENTAL_DEBUG_LLVM_IR=$(BUILD_FOLDER)/libmotoko_rts_incremental_debug.ll

CORE_DEBUG_LLVM_IR=$(BUILD_FOLDER)/core_debug.ll
CORE_RELEASE_LLVM_IR=$(BUILD_FOLDER)/core.ll

ALLOC_DEBUG_LLVM_IR=$(BUILD_FOLDER)/alloc_debug.ll
ALLOC_RELEASE_LLVM_IR=$(BUILD_FOLDER)/alloc.ll

COMPILER_BUILTINS_DEBUG_LLVM_IR=$(BUILD_FOLDER)/compiler_builtins_debug.ll
COMPILER_BUILTINS_RELEASE_LLVM_IR=$(BUILD_FOLDER)/compiler_builtins.ll

$(RTS_RUST_NON_INCREMENTAL_LLVM_IR): $(RTS_DEPENDENCIES)
	rm -rf $(RTS_RELEASE_TARGET_FOLDER)
	$(RTS_RELEASE_BUILD)
	cp $(CORE_RELEASE_TARGET) -T $(CORE_RELEASE_LLVM_IR)
	cp $(ALLOC_RELEASE_TARGET) -T $(ALLOC_RELEASE_LLVM_IR)
	cp $(COMPILER_BUILTINS_RELEASE_TARGET) -T $(COMPILER_BUILTINS_RELEASE_LLVM_IR)
	cp $(RTS_RELEASE_TARGET) -T $@

$(RTS_RUST_NON_INCREMENTAL_DEBUG_LLVM_IR): $(RTS_DEPENDENCIES)
	rm -rf $(RTS_DEBUG_TARGET_FOLDER) 
	$(RTS_DEBUG_BUILD)
	cp $(CORE_DEBUG_TARGET) -T $(CORE_DEBUG_LLVM_IR)
	cp $(ALLOC_DEBUG_TARGET) -T $(ALLOC_DEBUG_LLVM_IR)
	cp $(COMPILER_BUILTINS_DEBUG_TARGET) -T $(COMPILER_BUILTINS_DEBUG_LLVM_IR)
	cp $(RTS_DEBUG_TARGET) -T $@

$(RTS_RUST_INCREMENTAL_LLVM_IR): $(RTS_DEPENDENCIES)
	rm -rf $(RTS_RELEASE_TARGET_FOLDER)
	$(RTS_RELEASE_BUILD) --features incremental_gc
	cp $(CORE_RELEASE_TARGET) -T $(CORE_RELEASE_LLVM_IR)
	cp $(ALLOC_RELEASE_TARGET) -T $(ALLOC_RELEASE_LLVM_IR)
	cp $(COMPILER_BUILTINS_RELEASE_TARGET) -T $(COMPILER_BUILTINS_RELEASE_LLVM_IR)
	cp $(RTS_RELEASE_TARGET) -T $@

$(RTS_RUST_INCREMENTAL_DEBUG_LLVM_IR): $(RTS_DEPENDENCIES)
	rm -rf $(RTS_DEBUG_TARGET_FOLDER)
	$(RTS_DEBUG_BUILD) --features incremental_gc
	cp $(CORE_DEBUG_TARGET) -T $(CORE_DEBUG_LLVM_IR)
	cp $(ALLOC_DEBUG_TARGET) -T $(ALLOC_DEBUG_LLVM_IR)
	cp $(COMPILER_BUILTINS_DEBUG_TARGET) -T $(COMPILER_BUILTINS_DEBUG_LLVM_IR)
	cp $(RTS_DEBUG_TARGET) -T $@

RTS_RUST_NON_INCREMENTAL_WASM_A=$(BUILD_FOLDER)/libmotoko_rts.o
RTS_RUST_NON_INCREMENTAL_DEBUG_WASM_A=$(BUILD_FOLDER)/libmotoko_rts_debug.o

RTS_RUST_INCREMENTAL_WASM_A=$(BUILD_FOLDER)/libmotoko_rts_incremental.o
RTS_RUST_INCREMENTAL_DEBUG_WASM_A=$(BUILD_FOLDER)/libmotoko_rts_incremental_debug.o

CORE_WASM_A=$(BUILD_FOLDER)/core.o
CORE_DEBUG_WASM_A=$(BUILD_FOLDER)/core_debug.o

ALLOC_WASM_A=$(BUILD_FOLDER)/alloc.o
ALLOC_DEBUG_WASM_A=$(BUILD_FOLDER)/alloc_debug.o

COMPILER_BUILTINS_WASM_A=$(BUILD_FOLDER)/compiler_builtins.o
COMPILER_BUILTINS_DEBUG_WASM_A=$(BUILD_FOLDER)/compiler_builtins_debug.o

$(RTS_RUST_NON_INCREMENTAL_WASM_A): $(RTS_RUST_NON_INCREMENTAL_LLVM_IR)
	$(RTS_EMSCRIPTEN_RELEASE) -o $@ $+

$(RTS_RUST_NON_INCREMENTAL_DEBUG_WASM_A): $(RTS_RUST_NON_INCREMENTAL_DEBUG_LLVM_IR)
	$(RTS_EMSCRIPTEN_DEBUG) -o $@ $+

$(RTS_RUST_INCREMENTAL_WASM_A): $(RTS_RUST_INCREMENTAL_LLVM_IR)
	$(RTS_EMSCRIPTEN_RELEASE) -o $@ $+

$(RTS_RUST_INCREMENTAL_DEBUG_WASM_A): $(RTS_RUST_INCREMENTAL_DEBUG_LLVM_IR)
	$(RTS_EMSCRIPTEN_DEBUG) -o $@ $+

$(CORE_WASM_A): $(CORE_RELEASE_LLVM_IR)
	$(RTS_EMSCRIPTEN_RELEASE) -o $@ $+

$(CORE_DEBUG_WASM_A): $(CORE_DEBUG_LLVM_IR)
	$(RTS_EMSCRIPTEN_DEBUG) -o $@ $+

$(ALLOC_WASM_A): $(ALLOC_RELEASE_LLVM_IR)
	$(RTS_EMSCRIPTEN_RELEASE) -o $@ $+

$(ALLOC_DEBUG_WASM_A): $(ALLOC_DEBUG_LLVM_IR)
	$(RTS_EMSCRIPTEN_DEBUG) -o $@ $+

$(COMPILER_BUILTINS_WASM_A): $(COMPILER_BUILTINS_RELEASE_LLVM_IR)
	$(RTS_EMSCRIPTEN_RELEASE) -o $@ $+

$(COMPILER_BUILTINS_DEBUG_WASM_A): $(COMPILER_BUILTINS_DEBUG_LLVM_IR)
	$(RTS_EMSCRIPTEN_DEBUG) -o $@ $+

#
# The test suite
#

TEST_DEPENDENCIES=$(TOMMATH_WASM_A) $(TOMMATH_BINDINGS_RS)
TEST_BUILD=cd motoko-rts-tests && cargo build --target=wasm64-unknown-unknown -Zbuild-std=core,alloc,std,panic_abort
TEST_RUN=wasmtime --disable-cache --wasm-features=memory64 motoko-rts-tests/target/wasm64-unknown-unknown/debug/motoko-rts-tests.wasm

.PHONY: test

test: test-non-incremental test-incremental

test-non-incremental: $(TEST_DEPENDENCIES)
	$(TEST_BUILD)
	$(TEST_RUN)

test-incremental: $(TEST_DEPENDENCIES)
	$(TEST_BUILD) --features incremental_gc
	$(TEST_RUN)

#
# Putting it all together
#

# These symbols from musl are used by the code generator directly
EXPORTED_SYMBOLS=\
   __wasm_call_ctors \
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

WASM_A_DEPENDENCIES=$(TOMMATH_WASM_A) $(MUSL_WASM_A)
LINKER_OPTIONS=\
  -mwasm64 --import-memory --shared --no-entry --gc-sections \
  $(EXPORTED_SYMBOLS:%=--export=%) \
  --whole-archive

mo-rts.wasm: $(RTS_RUST_NON_INCREMENTAL_WASM_A) $(CORE_WASM_A) $(ALLOC_WASM_A) $(COMPILER_BUILTINS_WASM_A) $(WASM_A_DEPENDENCIES)
	$(WASM_LD) -o $@ \
		$(LINKER_OPTIONS) \
		$+

mo-rts-debug.wasm: $(RTS_RUST_NON_INCREMENTAL_DEBUG_WASM_A) $(CORE_DEBUG_WASM_A) $(ALLOC_DEBUG_WASM_A) $(COMPILER_BUILTINS_DEBUG_WASM_A) $(WASM_A_DEPENDENCIES)
	$(WASM_LD) -o $@ \
		$(LINKER_OPTIONS) \
		$+

mo-rts-incremental.wasm: $(RTS_RUST_INCREMENTAL_WASM_A) $(CORE_WASM_A) $(ALLOC_WASM_A) $(COMPILER_BUILTINS_WASM_A) $(WASM_A_DEPENDENCIES)
	$(WASM_LD) -o $@ \
		$(LINKER_OPTIONS) \
		$+

mo-rts-incremental-debug.wasm: $(RTS_RUST_INCREMENTAL_DEBUG_WASM_A) $(CORE_DEBUG_WASM_A) $(ALLOC_DEBUG_WASM_A) $(COMPILER_BUILTINS_DEBUG_WASM_A) $(WASM_A_DEPENDENCIES)
	$(WASM_LD) -o $@ \
		$(LINKER_OPTIONS) \
		$+

format:
	cargo fmt --verbose --manifest-path motoko-rts/Cargo.toml
	cargo fmt --verbose --manifest-path motoko-rts-tests/Cargo.toml
	cargo fmt --verbose --manifest-path motoko-rts-macros/Cargo.toml

clean:
	rm -rf \
	  _build \
	  mo-rts.wasm \
	  mo-rts-debug.wasm \
	  mo-rts-incremental.wasm \
	  mo-rts-incremental-debug.wasm \
	  motoko-rts/target \
	  motoko-rts-tests/target \
	  motoko-rts-macros/target \
	  motoko-rts/cargo-home \
