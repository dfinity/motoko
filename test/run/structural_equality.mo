assert({ x = 10 } == { x = 10 });
assert({ x = 10 } != { x = 9 });

assert(?10 == ?10);
assert(null == null);
assert(?10 != null);

assert(#x == #x);
assert(#ok(10) == #ok(10));
assert(#x != #y);
assert(#ok(10) != #ok(9));
