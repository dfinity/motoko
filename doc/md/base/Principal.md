# Principal
IC principals (user and canister smart contract IDs)

## Type `Principal`
`type Principal = Prim.Types.Principal`

Internet Computer principal identifiers.
Convert to `Blob` for access to bytes.

## Value `fromActor`
`let fromActor : (a : actor {  }) -> Principal`

Conversion.

## Value `toBlob`
`let toBlob : (p : Principal) -> Blob`

Conversion.

## Value `fromBlob`
`let fromBlob : (b : Blob) -> Principal`

Conversion.

## Function `toText`
`func toText(p : Principal) : Text`

Conversion.

## Function `isAnonymous`
`func isAnonymous(p : Principal) : Bool`


## Function `hash`
`func hash(principal : Principal) : Hash.Hash`


## Function `fromText`
`func fromText(t : Text) : Principal`


## Function `equal`
`func equal(x : Principal, y : Principal) : Bool`

Returns `x == y`.

## Function `notEqual`
`func notEqual(x : Principal, y : Principal) : Bool`

Returns `x != y`.

## Function `less`
`func less(x : Principal, y : Principal) : Bool`

Returns `x < y`.

## Function `lessOrEqual`
`func lessOrEqual(x : Principal, y : Principal) : Bool`

Returns `x <= y`.

## Function `greater`
`func greater(x : Principal, y : Principal) : Bool`

Returns `x > y`.

## Function `greaterOrEqual`
`func greaterOrEqual(x : Principal, y : Principal) : Bool`

Returns `x >= y`.

## Function `compare`
`func compare(x : Principal, y : Principal) : {#less; #equal; #greater}`

Returns the order of `x` and `y`.
