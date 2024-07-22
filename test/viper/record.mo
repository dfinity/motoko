// @verify

import Prim "mo:⛔";

actor Record {
  type R  = { aa: Int; b: Text; };
  type R1 = { aa: Int; b: Text; };

  let fld1: R = { aa = 42; b = "abacaba" };
  var fld2: R = { aa = 42; b = "abacaba" };

  let empty: R = { aa = 0; b = "" };

  let aa: Int = 42; // A field to check that "moc" handles disambiguation in record fields

  func array_record() {
    let arr: [var R] = [var empty, empty];
    arr[0] := { aa = 100; b = "aaa" };
    arr[1] := { aa = 200; b = "bbb" };

    assert:system arr[0] == { aa = 100; b = "aaa" } and arr[1] == { aa = 200; b = "bbb" };
  };

  func tuple_record() {
    let tup: (R, R) = ({ aa = 100; b = "aaa" }, { aa = 200; b = "bbb" });

    assert:system tup.0 == { aa = 100; b = "aaa" } and tup.1 == { aa = 200; b = "bbb" };
  };

  func get_record(): R {
    assert:return Prim.Ret<R>() == { aa = 100; b = "aaa" };
    return { aa = 100; b = "aaa" };
  };

  func call_record() {
    let res = get_record();
    assert:system res.b == "aaa";
  };

  func record_arg(r: R): R {
    assert:func r.aa == 100;
    assert:return Prim.Ret<R>().aa == 100;
    return { aa = 100; b = "aaa" };
  };

  func pass_record() {
    let res = record_arg({ aa = 100; b = "42" });
    assert:system res.aa == 100;
  };

  func match_on_record(): Int {
    assert:return Prim.Ret<Int>() == 42;

    let r = get_record();
    switch r {
      case { aa = number; b } {
        let c = b; // Check that "b" in scope
        return number - 58;
      }
    }
  };

  func record_types_are_structural() {
    let r:  R  = { aa = 100; b = "aaa" };
    let r1: R1 = { aa = 100; b = "aaa" };

    assert:system r == r1;
  };

  type RecursiveRecord1 = { x: ?RecursiveRecord1; y: ?RecursiveRecord2 };
  type RecursiveRecord2 = { x: ?RecursiveRecord1; };
};
