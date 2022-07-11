# Principal
IC principals (user and canister smart contract IDs)

## Type `Principal`
``` motoko norepl
type Principal = Prim.Types.Principal
```

Internet Computer principal identifiers.
Convert to `Blob` for access to bytes.

## Value `fromActor`
``` motoko norepl
let fromActor : (a : actor {  }) -> Principal
```

Conversion.

## Value `toBlob`
``` motoko norepl
let toBlob : (p : Principal) -> Blob
```

Conversion.

## Value `fromBlob`
``` motoko norepl
let fromBlob : (b : Blob) -> Principal
```

Conversion.

## Function `toText`
``` motoko norepl
func toText(p : Principal) : Text
```

Conversion.

## Function `isAnonymous`
``` motoko norepl
func isAnonymous(p : Principal) : Bool
```


## Function `hash`
``` motoko norepl
func hash(principal : Principal) : Hash.Hash
```


## Function `fromText`
``` motoko norepl
func fromText(t : Text) : Principal
```


## Function `equal`
``` motoko norepl
func equal(x : Principal, y : Principal) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko norepl
func notEqual(x : Principal, y : Principal) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko norepl
func less(x : Principal, y : Principal) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko norepl
func lessOrEqual(x : Principal, y : Principal) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko norepl
func greater(x : Principal, y : Principal) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko norepl
func greaterOrEqual(x : Principal, y : Principal) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko norepl
func compare(x : Principal, y : Principal) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.
