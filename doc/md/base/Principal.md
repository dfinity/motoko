# Principal
IC principals (user and canister smart contract IDs)

## Type `Principal`
``` motoko no-repl
type Principal = Prim.Types.Principal
```

Internet Computer principal identifiers.
Convert to `Blob` for access to bytes.

## Value `fromActor`
``` motoko no-repl
let fromActor : (a : actor {  }) -> Principal
```

Conversion.

## Value `toBlob`
``` motoko no-repl
let toBlob : (p : Principal) -> Blob
```

Conversion.

## Value `fromBlob`
``` motoko no-repl
let fromBlob : (b : Blob) -> Principal
```

Conversion.

## Function `toText`
``` motoko no-repl
func toText(p : Principal) : Text
```

Conversion.

## Function `isAnonymous`
``` motoko no-repl
func isAnonymous(p : Principal) : Bool
```


## Function `hash`
``` motoko no-repl
func hash(principal : Principal) : Hash.Hash
```


## Function `fromText`
``` motoko no-repl
func fromText(t : Text) : Principal
```


## Function `equal`
``` motoko no-repl
func equal(x : Principal, y : Principal) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko no-repl
func notEqual(x : Principal, y : Principal) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko no-repl
func less(x : Principal, y : Principal) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(x : Principal, y : Principal) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko no-repl
func greater(x : Principal, y : Principal) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(x : Principal, y : Principal) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko no-repl
func compare(x : Principal, y : Principal) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.
