# Bool
Boolean type and operations.

While boolean operators `_ and _` and `_ or _` are short-circuiting,
avoiding computation of the right argument when possible, the functions
`logand(_, _)` and `logor(_, _)` are *strict* and will always evaluate *both*
of their arguments.

## Type `Bool`

``` motoko
type Bool = Prim.Types.Bool
```

Booleans with constants `true` and `false`.

## Function `toText`

``` motoko
func toText(x : Bool) : Text
```

Conversion.

## Function `logand`

``` motoko
func logand(x : Bool, y : Bool) : Bool
```

Returns `x and y`.

## Function `logor`

``` motoko
func logor(x : Bool, y : Bool) : Bool
```

Returns `x or y`.

## Function `logxor`

``` motoko
func logxor(x : Bool, y : Bool) : Bool
```

Returns exclusive or of `x` and `y`, `x != y`.

## Function `lognot`

``` motoko
func lognot(x : Bool) : Bool
```

Returns `not x`.

## Function `equal`

``` motoko
func equal(x : Bool, y : Bool) : Bool
```

Returns `x == y`.

## Function `notEqual`

``` motoko
func notEqual(x : Bool, y : Bool) : Bool
```

Returns `x != y`.

## Function `compare`

``` motoko
func compare(x : Bool, y : Bool) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`, where `false < true`.
