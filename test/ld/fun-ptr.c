__attribute__ ((visibility("default")))
int f0(int x, int y)
{
    return x + y;
}

void* result_f1 = &f0;

__attribute__ ((visibility("default")))
int (*f1(void)) (int x, int y)
{
    return result_f1;
}

void* result_f2 = &f1;

__attribute__ ((visibility("default")))
void* f2()
{
    return result_f2;
}
