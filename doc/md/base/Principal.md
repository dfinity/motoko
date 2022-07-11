# Principal
IC principals (user and canister smart contract IDs)

## Type `Principal`
``` motoko
type Principal = Prim.Types.Principal
```

Internet Computer principal identifiers.
Convert to `Blob` for access to bytes.

## Value `fromActor`
``` motoko
let fromActor : (a : actor {  }) -> Principal
```

Conversion.

## Value `toBlob`
``` motoko
let toBlob : (p : Principal) -> Blob
```

Conversion.

## Value `fromBlob`
``` motoko
let fromBlob : (b : Blob) -> Principal
```

Conversion.

## Function `toText`
``` motoko
func toText(p : Principal) : Text
```

Conversion.

## Function `isAnonymous`
``` motoko
func isAnonymous(p : Principal) : Bool
```


## Function `hash`
``` motoko
func hash(principal : Principal) : Hash.Hash
```


## Function `fromText`
``` motoko
func fromText(t : Text) : Principal
```


## Function `equal`
``` motoko
func equal(x : Principal, y : Principal) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko
func notEqual(x : Principal, y : Principal) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko
func less(x : Principal, y : Principal) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko
func lessOrEqual(x : Principal, y : Principal) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko
func greater(x : Principal, y : Principal) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko
func greaterOrEqual(x : Principal, y : Principal) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko
func compare(x : Principal, y : Principal) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.
