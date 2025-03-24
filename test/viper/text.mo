// @verify

import Prim "mo:⛔";

actor Text {
  let fld1 = "42";
  var fld2 = "42";

  func concat_text() {
    let x = "foo";
    let y = "bar";
    let z = x # y;

    assert:system z == "foo" # "bar";
  };

  func array_text() {
    let arr: [var Text] = [var "", ""];
    arr[0] := "foo";
    arr[1] := "bar";

    assert:system arr[0] == "foo" and arr[1] == "bar";
  };

  func tuple_text() {
    let tup: (Text, Text) = ("foo", "bar");

    assert:system tup.0 == "foo" and tup.1 == "bar";
  };

  func get_text(): Text {
    assert:return Prim.Ret<Text>() == "foo";
    return "foo";
  };

  func call_text() {
    let res = get_text();
    assert:system res == "foo";
  };

  func change_field(): async () {
    let x = fld1;
    fld2 := "1000";
  };

  func text_arg(txt: Text): Text {
    assert:func txt == "abc";
    assert:return Prim.Ret<Text>() == "abc" # "42";
    return txt # "42";
  };

  func pass_text() {
    let res = text_arg("abc");
    assert:system res == "abc" # "42";
  };

  func match_on_text(): Int {
    assert:return Prim.Ret<Int>() == 42;

    let txt = get_text();
    switch txt {
      case "foo" return 42;
      case "bar" return 100;
      case _ return 0;
    }
  };
}
