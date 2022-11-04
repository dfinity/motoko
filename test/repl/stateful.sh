#!/usr/bin/env bash
# Tests that the repl preserves the state across
# syntax and type errors
moc -i <<__END__
let x = 42;
let x = "foobar
let x = 1 + true;
assert (x == 42);
__END__
