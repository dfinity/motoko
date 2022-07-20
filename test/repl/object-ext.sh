#!/usr/bin/env bash
# Tests that fields completely override base fields
moc -i <<__END__
{ { x = 42 : Nat }  |  x = -25 };

module X {public let a = 7};
{ { y = 3; z = "H" } and X | y = -25 };
__END__
