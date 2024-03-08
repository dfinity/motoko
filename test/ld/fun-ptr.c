__attribute__ ((visibility("default")))
int f0(int x, int y)
{
    return x + y;
}

// Clang/LLVM bug for Wasm `memory64`: &f0 generates a 32-bit value that is incompatible to void*
void* result_f1 = (void*)1; // &f0

__attribute__ ((visibility("default")))
int (*f1(void)) (int x, int y)
{
    return result_f1;
}

// Clang/LLVM bug for Wasm `memory64`: &f0 generates a 32-bit value that is incompatible to void*
void* result_f2 = (void*)2; // &f1

__attribute__ ((visibility("default")))
void *f2()
{
    return result_f2;
}
