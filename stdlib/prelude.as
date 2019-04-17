/**

Stdlib prelude
===============

 This prelude file proposes standard library features that _may_
belong in the _language_ (compiler-internal) prelude sometime, after
some further experience and discussion.  Until then, they live here.

*/

/***

 `nyi`: Not yet implemented
 -----------------------------

 Mark incomplete code with the `nyi` and `xxx` functions.

 Each have calls are well-typed in all typing contexts, which
trap in all execution contexts.

*/

func nyi() : None =
  { assert false ; nyi(); };

func xxx() : None =
  { assert false ; xxx(); };

/***

 `unreachable`
 --------------------

 Mark unreachable code with the `unreachable` function.

 Calls are well-typed in all typing contexts, and they
 trap in all execution contexts.

*/
func unreachable() : None = { assert false ; unreachable() };

/***

 `assertSome`
 --------------------

 Assert that the given value is not `null`; ignore this value and return unit.

*/
func assertSome<X>( x : ?X ) = {
  switch x {
    case null { unreachable() };
    case (?_) { };
  }
};

/***

 `assertNull`
 --------------------

 Assert that the given value is `null`; ignore this value and return unit.

*/
func assertNull<X>( x : ?X ) = {
  switch x {
    case null { };
    case (?_) { unreachable() };
  }
};

/***

 `printOpInt`
 --------------------

 Print an optional integer.

*/
func printOpInt( x : ?Int ) = {
  switch x {
    case null  { print "null" };
    case (?x_) { print "?"; printInt x_ };
  }
};

