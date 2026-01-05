# core/Bool
Boolean type and operations.

Import from the core package to use this module.
```motoko name=import
import Bool "mo:core/Bool";
```

While boolean operators `_ and _` and `_ or _` are short-circuiting,
avoiding computation of the right argument when possible, the functions
`logicalAnd(_, _)` and `logicalOr(_, _)` are *strict* and will always evaluate *both*
of their arguments.

Example:
```motoko include=import
let t = true;
let f = false;

// Short-circuiting AND
assert not (t and f);

// Short-circuiting OR
assert t or f;
```

## Type `Bool`
``` motoko no-repl
type Bool = Prim.Types.Bool
```

Booleans with constants `true` and `false`.

## Function `logicalAnd`
``` motoko no-repl
func logicalAnd(self : Bool, other : Bool) : Bool
```

Returns `a and b`.

Example:
```motoko include=import
assert not Bool.logicalAnd(true, false);
assert Bool.logicalAnd(true, true);
```

## Function `logicalOr`
``` motoko no-repl
func logicalOr(self : Bool, other : Bool) : Bool
```

Returns `a or b`.

Example:
```motoko include=import
assert Bool.logicalOr(true, false);
assert Bool.logicalOr(false, true);
```

## Function `logicalXor`
``` motoko no-repl
func logicalXor(self : Bool, other : Bool) : Bool
```

Returns exclusive or of `a` and `b`, `a != b`.

Example:
```motoko include=import
assert Bool.logicalXor(true, false);
assert not Bool.logicalXor(true, true);
assert not Bool.logicalXor(false, false);
```

## Function `logicalNot`
``` motoko no-repl
func logicalNot(self : Bool) : Bool
```

Returns `not bool`.

Example:
```motoko include=import
assert Bool.logicalNot(false);
assert not Bool.logicalNot(true);
```

## Function `equal`
``` motoko no-repl
func equal(self : Bool, other : Bool) : Bool
```

Returns `a == b`.

Example:
```motoko include=import
assert Bool.equal(true, true);
assert not Bool.equal(true, false);
```

## Function `compare`
``` motoko no-repl
func compare(self : Bool, other : Bool) : Order.Order
```

Returns the ordering of `a` compared to `b`.
Returns `#less` if `a` is `false` and `b` is `true`,
`#equal` if `a` equals `b`,
and `#greater` if `a` is `true` and `b` is `false`.

Example:
```motoko include=import
assert Bool.compare(true, false) == #greater;
assert Bool.compare(true, true) == #equal;
assert Bool.compare(false, true) == #less;
```

## Function `toText`
``` motoko no-repl
func toText(self : Bool) : Text
```

Returns a text value which is either `"true"` or `"false"` depending on the input value.

Example:
```motoko include=import
assert Bool.toText(true) == "true";
assert Bool.toText(false) == "false";
```

## Function `allValues`
``` motoko no-repl
func allValues() : Iter.Iter<Bool>
```

Returns an iterator over all possible boolean values (`true` and `false`).

Example:
```motoko include=import
let iter = Bool.allValues();
assert iter.next() == ?true;
assert iter.next() == ?false;
assert iter.next() == null;
```
