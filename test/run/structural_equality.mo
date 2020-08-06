assert({ x = 10 } == { x = 10 });
assert({ x = 10 } != { x = 9 });

assert(?10 == ?10);
assert(null == null);
assert(?10 != null);
