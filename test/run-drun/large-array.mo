// Should allocate 1G
// ignore(Array_init<()>(1024*1024*1024/4, ()));

// Should allocate 50MB (easier on CI)
ignore(Array_init<()>(50*1024*1024/4, ()));

//SKIP run
//SKIP run-ir
//SKIP run-low
