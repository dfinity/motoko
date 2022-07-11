# Result
Error handling with the Result type.

## Type `Result`
``` motoko
type Result<Ok, Err> = {#ok : Ok; #err : Err}
```

`Result<Ok, Err>` is the type used for returning and propagating errors. It
is a type with the variants, `#ok(Ok)`, representing success and containing
a value, and `#err(Err)`, representing error and containing an error value.

The simplest way of working with `Result`s is to pattern match on them:

For example, given a function `createUser(user : User) : Result<Id, String>`
where `String` is an error message we could use it like so:
```
switch(createUser(myUser)) {
  case #ok(id) Debug.print("Created new user with id: " # id)
  case #err(msg) Debug.print("Failed to create user with the error: " # msg)
}
```

## Function `equal`
``` motoko
func equal<Ok, Err>(eqOk : (Ok, Ok) -> Bool, eqErr : (Err, Err) -> Bool, r1 : Result<Ok, Err>, r2 : Result<Ok, Err>) : Bool
```


## Function `compare`
``` motoko
func compare<Ok, Err>(compareOk : (Ok, Ok) -> Order.Order, compareErr : (Err, Err) -> Order.Order, r1 : Result<Ok, Err>, r2 : Result<Ok, Err>) : Order.Order
```


## Function `chain`
``` motoko
func chain<R1, R2, Error>(x : Result<R1, Error>, y : R1 -> Result<R2, Error>) : Result<R2, Error>
```

Allows sequencing of `Result` values and functions that return
`Result`'s themselves.
```motoko
import Result "mo:base/Result";
type Result<T,E> = Result.Result<T, E>;
func largerThan10(x : Nat) : Result<Nat, Text> =
  if (x > 10) { #ok(x) } else { #err("Not larger than 10.") };

func smallerThan20(x : Nat) : Result<Nat, Text> =
  if (x < 20) { #ok(x) } else { #err("Not smaller than 20.") };

func between10And20(x : Nat) : Result<Nat, Text> =
  Result.chain(largerThan10(x), smallerThan20);

assert(between10And20(15) == #ok(15));
assert(between10And20(9) == #err("Not larger than 10."));
assert(between10And20(21) == #err("Not smaller than 20."));
```

## Function `flatten`
``` motoko
func flatten<Ok, Error>(result : Result<Result<Ok, Error>, Error>) : Result<Ok, Error>
```

Flattens a nested Result.

```motoko
import Result "mo:base/Result";
assert(Result.flatten<Nat, Text>(#ok(#ok(10))) == #ok(10));
assert(Result.flatten<Nat, Text>(#err("Wrong")) == #err("Wrong"));
assert(Result.flatten<Nat, Text>(#ok(#err("Wrong"))) == #err("Wrong"));
```

## Function `mapOk`
``` motoko
func mapOk<Ok1, Ok2, Error>(x : Result<Ok1, Error>, f : Ok1 -> Ok2) : Result<Ok2, Error>
```

Maps the `Ok` type/value, leaving any `Error` type/value unchanged.

## Function `mapErr`
``` motoko
func mapErr<Ok, Error1, Error2>(x : Result<Ok, Error1>, f : Error1 -> Error2) : Result<Ok, Error2>
```

Maps the `Err` type/value, leaving any `Ok` type/value unchanged.

## Function `fromOption`
``` motoko
func fromOption<R, E>(x : ?R, err : E) : Result<R, E>
```

Create a result from an option, including an error value to handle the `null` case.
```motoko
import Result "mo:base/Result";
assert(Result.fromOption(?42, "err") == #ok(42));
assert(Result.fromOption(null, "err") == #err("err"));
```

## Function `toOption`
``` motoko
func toOption<R, E>(r : Result<R, E>) : ?R
```

Create an option from a result, turning all #err into `null`.
```motoko
import Result "mo:base/Result";
assert(Result.toOption(#ok(42)) == ?42);
assert(Result.toOption(#err("err")) == null);
```

## Function `iterate`
``` motoko
func iterate<Ok, Err>(res : Result<Ok, Err>, f : Ok -> ())
```

Applies a function to a successful value, but discards the result. Use
`iterate` if you're only interested in the side effect `f` produces.

```motoko
import Result "mo:base/Result";
var counter : Nat = 0;
Result.iterate<Nat, Text>(#ok(5), func (x : Nat) { counter += x });
assert(counter == 5);
Result.iterate<Nat, Text>(#err("Wrong"), func (x : Nat) { counter += x });
assert(counter == 5);
```

## Function `isOk`
``` motoko
func isOk(r : Result<Any, Any>) : Bool
```


## Function `isErr`
``` motoko
func isErr(r : Result<Any, Any>) : Bool
```


## Function `assertOk`
``` motoko
func assertOk(r : Result<Any, Any>)
```

Asserts that its argument is an `#ok` result, traps otherwise.

## Function `assertErr`
``` motoko
func assertErr(r : Result<Any, Any>)
```

Asserts that its argument is an `#err` result, traps otherwise.
