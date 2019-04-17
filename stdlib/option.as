/**

Functions for Option types.

*/

/***

 `isSome`
 --------------------

 Returns true if the value is not `null`.

*/
func isSome(ox: ?Any) : Bool {
  switch ox {
    case (null) false;
    case (?_) true;
  }
};

/***

 `isNull`
 --------------------

 Returns true if the value is `null`.

*/
func isNull(ox: ?Any) : Bool = not isSome(ox);

/***

 `unwrap`
 --------------------

 Unwrap an optional value, and fail if it is `null`.

*/
func unwrap<T>(ox:?T) : T {
  switch ox {
    case (null) { unreachable() };
    case (?x) x;
  }
};

/***

 `unwrapOr`
 --------------------

 Unwrap an optional value or a default.

*/
func unwrapOr<T>(ox:?T, d:T) : T {
  switch ox {
    case (null) { d };
    case (?x) x;
  }
};

/***

 `option`
 --------------------

 Unwrap an optional value. If null, return the default, else, apply the function to the unwrapped value.

*/
func option<F, T>(ox:?F, f:F->T, d:T) : T {
  switch ox {
    case (null) { d };
    case (?x) f(x);
  }
};
