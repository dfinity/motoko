// No unboxing between the start of foo and the call to serialize
// CHECK: (func $foo
// CHECK-NOT: box_reference
// CHECK: call $@deserialize<Text>
shared func foo(a : Text, b: Int) {};

// No boxing between the call to serialize and the indirect call
// CHECK: (func $start
// CHECK: call $@serialize<Text>
// CHECK-NOT: box_reference
// CHECK: call_indirect
foo("a", 42);

