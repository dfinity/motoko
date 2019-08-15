#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <errno.h>

#include "rts.h"
#include "wasm-rt-impl.h"

typedef u32 ref;

typedef struct databuf {
  u32 len;
  u8 *data;
} databuf;

void *refs[1000];
int ref_counter = 0;

bool ok_to_trap = false;

#define PAGE_SIZE 65536
wasm_rt_memory_t mem;
wasm_rt_table_t table;
u32 memory_base = PAGE_SIZE;
u32 stack_pointer = PAGE_SIZE;

void idl_trap() {
  printf("IDL trap\n");
  fflush(stdout);
  exit(1);
}
void bigint_trap() {
  printf("bigint trap\n");
  fflush(stdout);
  exit(1);
}

u32 heap_ptr = PAGE_SIZE;
u32 alloc_bytes(u32 n) {
  if (heap_ptr + n > mem.pages * PAGE_SIZE) {
    uint32_t r = wasm_rt_grow_memory(&mem, (heap_ptr + n + PAGE_SIZE - 1) % PAGE_SIZE);
    if (r == -1) {
      printf("cant grow memory\n");
      fflush(stdout);
      exit(1);
    }
  }
  u32 r = heap_ptr - 1; //skew pointer
  heap_ptr += n;
  return r;
}

u32 *(Z_envZ___stack_pointerZ_i) = &stack_pointer;
u32 *(Z_envZ___memory_baseZ_i) = &memory_base;
wasm_rt_memory_t (*Z_envZ_memory) = &mem;
wasm_rt_table_t (*Z_envZ___indirect_function_table) = &table;
u32 table_base = 0;
u32 *(Z_envZ___table_baseZ_i) = &table_base;
void (*Z_envZ_idl_trapZ_vv)(void) = &idl_trap;
void (*Z_envZ_bigint_trapZ_vv)(void) = &bigint_trap;
u32 (*Z_envZ_alloc_bytesZ_ii)(u32) = &alloc_bytes;

int main(int argc, char** argv) {
  /* Make sure there is at least one command-line argument. */
  if (argc < 2) {
    printf("Usage: %s inputfile", argv[0]);
    return 1;
  }

  // read file
  FILE *f = fopen(argv[1], "rb");
  if (f == NULL) {
      fprintf(stderr, "Error opening file %s: %s\n", argv[1], strerror( errno ));
      exit(1);
  }
  fseek(f, 0, SEEK_END);
  long input_size = ftell(f);
  fseek(f, 0, SEEK_SET);  /* same as rewind(f); */
  char *input = malloc(input_size);
  fread(input, 1, input_size, f);
  fclose(f);

  wasm_rt_trap_t code = wasm_rt_impl_try();
  if (code != 0) {
    printf("A trap occurred with code: %d (trap ok? %d)\n", code, ok_to_trap);
    if (ok_to_trap) exit(0); else abort();
  }

  // Initialize memory
  wasm_rt_allocate_memory(&mem, 10, 10000);
  wasm_rt_allocate_table(&table, 1, 1);

  printf("Running now.\n");

  /* Initialize the fac module. Since we didn't define WASM_RT_MODULE_PREFIX,
  the initialization function is called `init`. */
  init();


  free(input);
  return 0;
}
