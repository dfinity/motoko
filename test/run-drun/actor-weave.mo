import { Ext } = "./actor-module";

actor Weave {
  public func beep() {}
} and Ext(42);

Weave.beep(); //OR-CALL ingress beep "DIDL\x00\x00"
Weave.foo(); //OR-CALL ingress foo "DIDL\x00\x00"

//SKIP run
//SKIP run-low
//SKIP run-ir
