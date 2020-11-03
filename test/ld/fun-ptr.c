__attribute__ ((visibility("default")))
int f0(int x, int y)
{
    return x + y;
}

__attribute__ ((visibility("default")))
int (*f1(void)) (int x, int y)
{
    return &f0;
}

__attribute__ ((visibility("default")))
void *f2()
{
    return &f1;
}
