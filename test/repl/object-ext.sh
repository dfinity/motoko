#!/usr/bin/env bash
# Tests that fields completely override base fields
moc -i <<__END__
{ x = -25 in { x = 42 : Nat } };

module X {public let a = 7};
{ y = -25 in { y = 3; z = "H" } and X };
__END__
