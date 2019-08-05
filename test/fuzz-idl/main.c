#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <errno.h>

/* Uncomment this to define fac_init and fac_Z_facZ_ii instead. */
/* #define WASM_RT_MODULE_PREFIX fac_ */

#include "test.h"
#include "wasm-rt-impl.h"

typedef u32 ref;

typedef struct databuf {
  u32 len;
  u8 *data;
} databuf;

void *refs[1000];
int ref_counter = 0;

bool ok_to_trap = false;


extern ref data_externalize(u32 offset, u32 len) {
  // printf("data_externalize: %d bytes\n", len);
  databuf *db = malloc(sizeof(databuf));
  if (len > 0 && db == NULL ) {
    fprintf(stderr, "data_externalize: OOM!\n");
    exit(1);
  }
  db->len = len;
  db->data = malloc(len+1);
  memcpy(db->data, Z_mem->data + offset, len);
  db->data[len] = '\0';
  refs[ref_counter] = db;
  return ref_counter++;
}

extern ref data_of_string(char *buf, size_t len) {
  // printf("data_of_string: %d bytes\n", len);
  databuf *db = malloc(sizeof(databuf));
  db->len = len;
  db->data = malloc(len+1);
  memcpy(db->data, buf, len);
  db->data[len] = '\0';
  refs[ref_counter] = db;
  return ref_counter++;
}

extern u32 data_length(ref dbref) {
  databuf *db = refs[dbref];
  // printf("data_length: %d bytes\n", db->len);
  return db->len;
}

extern void data_internalize(u32 dst, u32 len, ref dbref, u32 offset){
  // printf("data_internalize\n");
  databuf *db = refs[dbref];
  memcpy(Z_mem->data + dst, db->data + offset, len);
  return;
}

void test_print(ref dbref) {
  databuf *db = refs[dbref];
  // printf("test_print: %d bytes\n", db->len);
  if (memcmp(db->data, "IDL error:",strlen("IDL error:")) == 0) {
    ok_to_trap = true;
  };
  if (memcmp(db->data, "Cannot grow memory",strlen("Cannot grow memory")) == 0) {
    ok_to_trap = true;
  };
  printf("%s", db->data);
  return;
}

void (*Z_testZ_printZ_vi)(u32) = &test_print;;
u32 (*Z_dataZ_externalizeZ_iii)(u32, u32) = &data_externalize;
u32 (*Z_dataZ_lengthZ_ii)(u32) = &data_length;
void (*Z_dataZ_internalizeZ_viiii)(u32, u32, u32, u32) = &data_internalize;

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


  /* Initialize the fac module. Since we didn't define WASM_RT_MODULE_PREFIX,
  the initialization function is called `init`. */
  init();
  // the whole wasm2c embedding behaves badly when it really runs out of memory
  // so lets run out of memory earlier
  Z_mem->max_pages = 10000;
  Z_startZ_vv();

  ref db = data_of_string(input, input_size);
  Z_fooZ_vi(db);

  return 0;
}
