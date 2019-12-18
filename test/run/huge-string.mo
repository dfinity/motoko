/*
This uses repeated self-concatenation to build a huge string
that would fill up most of the memory if implemented naively.
It then checks that heap usage is actually not too bad,
showing that string concatenation works as a tree with sharing.
*/

var s = "Badger";
let n = 27;
for (i in range(1,n)) {
  s := s # s;
};
assert(rts_heap_size() < 2_000);

//SKIP run
//SKIP run-low
//SKIP run-ir
