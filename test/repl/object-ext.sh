#!/usr/bin/env bash
# Tests that fields completely override base fields
moc -i <<__END__
{ x = -25 in { x = 42 : Nat } };
__END__
