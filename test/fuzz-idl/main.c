#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <errno.h>

/* Uncomment this to define fac_init and fac_Z_facZ_ii instead. */
/* #define WASM_RT_MODULE_PREFIX fac_ */

#include "test.h"
#include "wasm-rt-impl.h"

u32 arg_len;
char *arg_data;

u32 ret_len;
char *ret_data;

bool ok_to_trap = false;


extern u32 msg_arg_data_size(u64 api_nonce) {
  return arg_len;
}
extern void msg_arg_data_copy(u64 api_nonce, u32 dest, u32 len, u32 offset) {
  memcpy(Z_mem->data + dest, arg_data + offset, len);
  return;
}

extern void msg_reply(u64 api_nonce, u32 src, u32 len) {
  ret_len = len;
  ret_data = malloc(len+1);
  memcpy(ret_data, Z_mem->data + src, len);
  return;
}


void debug_print(u32 src, u32 len) {
  char msg[len+1];
  memcpy(msg, Z_mem->data + src, len);
  msg[len] = '\0';
  if (memcmp(msg, "IDL error:",strlen("IDL error:")) == 0) {
    ok_to_trap = true;
  };
  if (memcmp(msg, "Cannot grow memory",strlen("Cannot grow memory")) == 0) {
    ok_to_trap = true;
  };
  if (memcmp(msg, "Array allocation too large",strlen("Array allocation too large")) == 0) {
    ok_to_trap = true;
  };
  printf("%s", msg);
  fflush(stdout);
  return;
}

void (*Z_debugZ_printZ_vii)(u32, u32) = &debug_print;
u32 (*Z_msgZ_arg_data_sizeZ_ij)(u64) = &msg_arg_data_size;
void (*Z_msgZ_arg_data_copyZ_vjiii)(u64, u32, u32, u32) = &msg_arg_data_copy;

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
  arg_len = ftell(f);
  fseek(f, 0, SEEK_SET);  /* same as rewind(f); */
  arg_data = malloc(arg_len);
  size_t read = fread(arg_data, 1, arg_len, f);
  if (read != arg_len) {
      fprintf(stderr, "Could not read all of %s\n", argv[1]);
      exit(1);
  }
  fclose(f);

  wasm_rt_trap_t code = wasm_rt_impl_try();
  if (code != 0) {
    printf("A trap occurred with code: %d (trap ok? %d)\n", code, ok_to_trap);
    if (ok_to_trap) exit(0); else abort();
  }


  /* Initialize the fac module. Since we didn't define WASM_RT_MODULE_PREFIX,
  the initialization function is called `init`. */
  init();
  // the whole wasm2c embedding behaves badly when it really runs out of memory
  // so lets run out of memory earlier
  Z_mem->max_pages = 10000;
  Z_canister_initZ_vj(0);

  Z_canister_updateZ20fooZ_vj(0);

  return 0;
}
