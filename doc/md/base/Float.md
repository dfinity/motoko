# Float
64-bit Floating-point numbers

## Type `Float`
``` motoko
type Float = Prim.Types.Float
```

64-bit floating point numbers.

## Value `pi`
``` motoko
let pi : Float
```

Ratio of the circumference of a circle to its diameter.

## Value `e`
``` motoko
let e : Float
```

Base of the natural logarithm.

## Value `abs`
``` motoko
let abs : (x : Float) -> Float
```

Returns the absolute value of `x`.

## Value `sqrt`
``` motoko
let sqrt : (x : Float) -> Float
```

Returns the square root of `x`.

## Value `ceil`
``` motoko
let ceil : (x : Float) -> Float
```

Returns the smallest integral float greater than or equal to `x`.

## Value `floor`
``` motoko
let floor : (x : Float) -> Float
```

Returns the largest integral float less than or equal to `x`.

## Value `trunc`
``` motoko
let trunc : (x : Float) -> Float
```

Returns the nearest integral float not greater in magnitude than `x`.

## Value `nearest`
``` motoko
let nearest : (x : Float) -> Float
```

Returns the nearest integral float to `x`.

## Value `copySign`
``` motoko
let copySign : (x : Float, y : Float) -> Float
```

Returns `x` if `x` and `y` have same sign, otherwise `x` with negated sign.

## Value `min`
``` motoko
let min : (x : Float, y : Float) -> Float
```

Returns the smaller value of `x` and `y`.

## Value `max`
``` motoko
let max : (x : Float, y : Float) -> Float
```

Returns the larger value of `x` and `y`.

## Value `sin`
``` motoko
let sin : (x : Float) -> Float
```

Returns the sine of the radian angle `x`.

## Value `cos`
``` motoko
let cos : (x : Float) -> Float
```

Returns the cosine of the radian angle `x`.

## Value `tan`
``` motoko
let tan : (x : Float) -> Float
```

Returns the tangent of the radian angle `x`.

## Value `arcsin`
``` motoko
let arcsin : (x : Float) -> Float
```

Returns the arc sine of `x` in radians.

## Value `arccos`
``` motoko
let arccos : (x : Float) -> Float
```

Returns the arc cosine of `x` in radians.

## Value `arctan`
``` motoko
let arctan : (x : Float) -> Float
```

Returns the arc tangent of `x` in radians.

## Value `arctan2`
``` motoko
let arctan2 : (y : Float, x : Float) -> Float
```

Given `(y,x)`, returns the arc tangent in radians of `y/x` based on the signs of both values to determine the correct quadrant.

## Value `exp`
``` motoko
let exp : (x : Float) -> Float
```

Returns the value of `e` raised to the `x`-th power.

## Value `log`
``` motoko
let log : (x : Float) -> Float
```

Returns the natural logarithm (base-`e`) of `x`.

## Function `format`
``` motoko
func format(fmt : {#fix : Nat8; #exp : Nat8; #gen : Nat8; #hex : Nat8; #exact}, x : Float) : Text
```

Formatting. `format(fmt, x)` formats `x` to `Text` according to the
formatting directive `fmt`, which can take one of the following forms:

* `#fix prec` as fixed-point format with `prec` digits
* `#exp prec` as exponential format with `prec` digits
* `#gen prec` as generic format with `prec` digits
* `#hex prec` as hexadecimal format with `prec` digits
* `#exact` as exact format that can be decoded without loss.

## Value `toText`
``` motoko
let toText : Float -> Text
```

Conversion to Text. Use `format(fmt, x)` for more detailed control.

## Value `toInt64`
``` motoko
let toInt64 : Float -> Int64
```

Conversion to Int64 by truncating Float, equivalent to `toInt64(trunc(f))`

## Value `fromInt64`
``` motoko
let fromInt64 : Int64 -> Float
```

Conversion from Int64.

## Value `toInt`
``` motoko
let toInt : Float -> Int
```

Conversion to Int.

## Value `fromInt`
``` motoko
let fromInt : Int -> Float
```

Conversion from Int. May result in `Inf`.

## Function `equal`
``` motoko
func equal(x : Float, y : Float) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko
func notEqual(x : Float, y : Float) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko
func less(x : Float, y : Float) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko
func lessOrEqual(x : Float, y : Float) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko
func greater(x : Float, y : Float) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko
func greaterOrEqual(x : Float, y : Float) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko
func compare(x : Float, y : Float) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

## Function `neq`
``` motoko
func neq(x : Float) : Float
```

Returns the negation of `x`, `-x` .

## Function `add`
``` motoko
func add(x : Float, y : Float) : Float
```

Returns the sum of `x` and `y`, `x + y`.

## Function `sub`
``` motoko
func sub(x : Float, y : Float) : Float
```

Returns the difference of `x` and `y`, `x - y`.

## Function `mul`
``` motoko
func mul(x : Float, y : Float) : Float
```

Returns the product of `x` and `y`, `x * y`.

## Function `div`
``` motoko
func div(x : Float, y : Float) : Float
```

Returns the division of `x` by `y`, `x / y`.

## Function `rem`
``` motoko
func rem(x : Float, y : Float) : Float
```

Returns the remainder of `x` divided by `y`, `x % y`.

## Function `pow`
``` motoko
func pow(x : Float, y : Float) : Float
```

Returns `x` to the power of `y`, `x ** y`.
