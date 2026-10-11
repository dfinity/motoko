//MOC-FLAG --plugin ../../../_build/compute-plugin-tests/echo
import Prim "mo:⛔";

assert Prim.fixtureEcho("") == "";
assert Prim.fixtureEcho("abc\00\ff") == "abc\00\ff";
let echo : Blob -> Blob = Prim.fixtureEcho;
let bytes = Prim.arrayToBlob(Prim.Array_tabulate<Nat8>(65_537,
  func(i) { Prim.natToNat8(i % 256) }));
assert echo(bytes) == bytes;
assert echo(echo(bytes)) == bytes;
