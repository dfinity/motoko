/*
For some reason, clang comes with files like intstd.h, but not with stdlib.h.

If we do not define this here, clang might pick up glibc's stdlib.h, which
tries to pull in files related to 32 bits, which are not always installed. And
installing 32 bit headers is also non-trivial. And using glibc's headers feels
wrong anyways when we don't actually use glibc.

So for now we stub this out manually (inspired by musl's stdlib.h).

At some point we might actually use a proper C library. Then this library will
provide the proper headers.
*/

#define _Addr int
#define _Int64 long long
#define NULL ((void*)0)
typedef unsigned _Addr size_t;

