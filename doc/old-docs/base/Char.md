# Char
Characters

## Type `Char`
``` motoko no-repl
type Char = Prim.Types.Char
```

Characters represented as Unicode code points.

## Value `toNat32`
``` motoko no-repl
let toNat32 : (c : Char) -> Nat32
```

Convert character `c` to a word containing its Unicode scalar value.

## Value `fromNat32`
``` motoko no-repl
let fromNat32 : (w : Nat32) -> Char
```

Convert `w` to a character.
Traps if `w` is not a valid Unicode scalar value.
Value `w` is valid if, and only if, `w < 0xD800 or (0xE000 <= w and w <= 0x10FFFF)`.

## Value `toText`
``` motoko no-repl
let toText : (c : Char) -> Text
```

Convert character `c` to single character text.

## Function `isDigit`
``` motoko no-repl
func isDigit(c : Char) : Bool
```

Returns `true` when `c` is a decimal digit between `0` and `9`, otherwise `false`.

## Value `isWhitespace`
``` motoko no-repl
let isWhitespace : (c : Char) -> Bool
```

Returns the Unicode _White_Space_ property of `c`.

## Value `isLowercase`
``` motoko no-repl
let isLowercase : (c : Char) -> Bool
```

Returns the Unicode _Lowercase_ property of `c`.

## Value `isUppercase`
``` motoko no-repl
let isUppercase : (c : Char) -> Bool
```

Returns the Unicode _Uppercase_ property of `c`.

## Value `isAlphabetic`
``` motoko no-repl
let isAlphabetic : (c : Char) -> Bool
```

Returns the Unicode _Alphabetic_ property of `c`.

## Function `equal`
``` motoko no-repl
func equal(x : Char, y : Char) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Char, y : Char) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko no-repl
func less(x : Char, y : Char) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Char, y : Char) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko no-repl
func greater(x : Char, y : Char) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Char, y : Char) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko no-repl
func compare(x : Char, y : Char) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.
