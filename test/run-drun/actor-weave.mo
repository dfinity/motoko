import { Ext } = "./actor-module";

actor Weave {
  public func beep() {}
} and Ext(42);

//ignore Weave.beep(); //OR-CALL ingress beep "DIDL\x00\x00"
//ignore Weave.foo(); //OR-CALL ingress foo "DIDL\x00\x00"

//SKIP run-low
//SKIP run-ir
