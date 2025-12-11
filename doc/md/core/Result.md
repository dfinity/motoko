# core/Result
Module for error handling with the Result type.

The Result type is used for returning and propagating errors. It has two variants:
`#ok(Ok)`, representing success and containing a value, and `#err(Err)`, representing
error and containing an error value.

Import from the core package to use this module.
```motoko name=import
import Result "mo:core/Result";
```

## Type `Result`
``` motoko no-repl
type Result<Ok, Err> = Types.Result<Ok, Err>
```

The Result type used for returning and propagating errors.

The simplest way of working with Results is to pattern match on them.
For example:
```motoko include=import
import Text "mo:core/Text";

type Email = Text;
type ErrorMessage = Text;

func validateEmail(email : Text) : Result.Result<Email, ErrorMessage> {
  let parts = Text.split(email, #char '@');
  let beforeAt = parts.next();
  let afterAt = parts.next();
  switch (beforeAt, afterAt) {
    case (?local, ?domain) {
      if (local == "") return #err("Username cannot be empty");
      if (not Text.contains(domain, #char '.')) return #err("Invalid domain format");
      #ok(email)
    };
    case _ #err("Email must contain exactly one @ symbol")
  }
};

assert validateEmail("user@example.com") == #ok("user@example.com");
assert validateEmail("invalid.email") == #err("Email must contain exactly one @ symbol");
assert validateEmail("@domain.com") == #err("Username cannot be empty");
assert validateEmail("user@invalid") == #err("Invalid domain format");
```
@deprecated M0235

## Function `equal`
``` motoko no-repl
func equal<Ok, Err>(self : Result<Ok, Err>, other : Result<Ok, Err>, equalOk : (implicit : (equal : Ok, Ok) -> Bool), equalErr : (implicit : (equal : (Err, Err) -> Bool))) : Bool
```

Compares two Results for equality.

Example:
```motoko include=import
import Nat "mo:core/Nat";
import Text "mo:core/Text";

let result1 = #ok 10;
let result2 = #ok 10;
let result3 = #err "error";

assert Result.equal<Nat, Text>(result1, result2, Nat.equal, Text.equal);
assert not Result.equal<Nat, Text>(result1, result3, Nat.equal, Text.equal);
```

## Function `compare`
``` motoko no-repl
func compare<Ok, Err>(self : Result<Ok, Err>, other : Result<Ok, Err>, compareOk : (implicit : (compare : (Ok, Ok) -> Order.Order)), compareErr : (implicit : (compare : (Err, Err) -> Order.Order))) : Order.Order
```

Compares two Result values. `#ok` is larger than `#err`. This ordering is
arbitrary, but it lets you for example use Results as keys in ordered maps.

Example:
```motoko include=import
import Nat "mo:core/Nat";
import Text "mo:core/Text";

let result1 = #ok 5;
let result2 = #ok 10;
let result3 = #err "error";

assert Result.compare<Nat, Text>(result1, result2, Nat.compare, Text.compare) == #less;
assert Result.compare<Nat, Text>(result2, result1, Nat.compare, Text.compare) == #greater;
assert Result.compare<Nat, Text>(result1, result3, Nat.compare, Text.compare) == #greater;
```

## Function `chain`
``` motoko no-repl
func chain<Ok1, Ok2, Err>(self : Result<Ok1, Err>, f : Ok1 -> Result<Ok2, Err>) : Result<Ok2, Err>
```

Allows sequencing of Result values and functions that return
Results themselves.
```motoko include=import
type Result<Ok,Err> = Result.Result<Ok, Err>;
func largerThan10(x : Nat) : Result<Nat, Text> =
  if (x > 10) { #ok(x) } else { #err("Not larger than 10.") };

func smallerThan20(x : Nat) : Result<Nat, Text> =
  if (x < 20) { #ok(x) } else { #err("Not smaller than 20.") };

func between10And20(x : Nat) : Result<Nat, Text> =
  Result.chain(largerThan10(x), smallerThan20);

assert between10And20(15) == #ok(15);
assert between10And20(9) == #err("Not larger than 10.");
assert between10And20(21) == #err("Not smaller than 20.");
```

## Function `flatten`
``` motoko no-repl
func flatten<Ok, Err>(self : Result<Result<Ok, Err>, Err>) : Result<Ok, Err>
```

Flattens a nested Result.

```motoko include=import
assert Result.flatten<Nat, Text>(#ok(#ok(10))) == #ok(10);
assert Result.flatten<Nat, Text>(#err("Wrong")) == #err("Wrong");
assert Result.flatten<Nat, Text>(#ok(#err("Wrong"))) == #err("Wrong");
```

## Function `mapOk`
``` motoko no-repl
func mapOk<Ok1, Ok2, Err>(self : Result<Ok1, Err>, f : Ok1 -> Ok2) : Result<Ok2, Err>
```

Maps the `Ok` type/value, leaving any `Err` type/value unchanged.

Example:
```motoko include=import
let result1 = #ok(42);
let result2 = #err("error");

let doubled1 = Result.mapOk<Nat, Nat, Text>(result1, func x = x * 2);
assert doubled1 == #ok(84);

let doubled2 = Result.mapOk<Nat, Nat, Text>(result2, func x = x * 2);
assert doubled2 == #err("error");
```

## Function `mapErr`
``` motoko no-repl
func mapErr<Ok, Err1, Err2>(self : Result<Ok, Err1>, f : Err1 -> Err2) : Result<Ok, Err2>
```

Maps the `Err` type/value, leaving any `Ok` type/value unchanged.

Example:
```motoko include=import
let result1 = #ok(42);
let result2 = #err("error");

let mapped1 = Result.mapErr<Nat, Text, Text>(result1, func x = x # "!");
assert mapped1 == #ok(42);

let mapped2 = Result.mapErr<Nat, Text, Text>(result2, func x = x # "!");
assert mapped2 == #err("error!");
```

## Function `fromOption`
``` motoko no-repl
func fromOption<Ok, Err>(x : ?Ok, err : Err) : Result<Ok, Err>
```

Create a result from an option, including an error value to handle the `null` case.
```motoko include=import
assert Result.fromOption(?42, "err") == #ok(42);
assert Result.fromOption(null, "err") == #err("err");
```

## Function `toOption`
``` motoko no-repl
func toOption<Ok, Err>(self : Result<Ok, Err>) : ?Ok
```

Create an option from a result, turning all #err into `null`.
```motoko include=import
assert Result.toOption(#ok(42)) == ?42;
assert Result.toOption(#err("err")) == null;
```

## Function `forOk`
``` motoko no-repl
func forOk<Ok, Err>(self : Result<Ok, Err>, f : Ok -> ())
```

Applies a function to a successful value and discards the result. Use
`forOk` if you're only interested in the side effect `f` produces.

```motoko include=import
var counter : Nat = 0;
Result.forOk<Nat, Text>(#ok(5), func (x : Nat) { counter += x });
assert counter == 5;
Result.forOk<Nat, Text>(#err("Error"), func (x : Nat) { counter += x });
assert counter == 5;
```

## Function `forErr`
``` motoko no-repl
func forErr<Ok, Err>(self : Result<Ok, Err>, f : Err -> ())
```

Applies a function to an error value and discards the result. Use
`forErr` if you're only interested in the side effect `f` produces.

```motoko include=import
var counter : Nat = 0;
Result.forErr<Nat, Text>(#err("Error"), func (x : Text) { counter += 1 });
assert counter == 1;
Result.forErr<Nat, Text>(#ok(5), func (x : Text) { counter += 1 });
assert counter == 1;
```

## Function `isOk`
``` motoko no-repl
func isOk(self : Result<Any, Any>) : Bool
```

Whether this Result is an `#ok`.

Example:
```motoko include=import
assert Result.isOk(#ok(42));
assert not Result.isOk(#err("error"));
```

## Function `isErr`
``` motoko no-repl
func isErr(self : Result<Any, Any>) : Bool
```

Whether this Result is an `#err`.

Example:
```motoko include=import
assert Result.isErr(#err("error"));
assert not Result.isErr(#ok(42));
```

## Function `assertOk`
``` motoko no-repl
func assertOk(self : Result<Any, Any>)
```

Asserts that its argument is an `#ok` result, traps otherwise.

Example:
```motoko include=import
Result.assertOk(#ok(42)); // succeeds
// Result.assertOk(#err("error")); // would trap
```

## Function `assertErr`
``` motoko no-repl
func assertErr(self : Result<Any, Any>)
```

Asserts that its argument is an `#err` result, traps otherwise.

Example:
```motoko include=import
Result.assertErr(#err("error")); // succeeds
// Result.assertErr(#ok(42)); // would trap
```

## Function `fromUpper`
``` motoko no-repl
func fromUpper<Ok, Err>(result : {#Ok : Ok; #Err : Err}) : Result<Ok, Err>
```

Converts an upper cased `#Ok`, `#Err` result type into a lowercased `#ok`, `#err` result type.
On the IC, a common convention is to use `#Ok` and `#Err` as the variants of a result type,
but in Motoko, we use `#ok` and `#err` instead.

Example:
```motoko include=import
let upper = #Ok(42);
let lower = Result.fromUpper(upper);
assert lower == #ok(42);
```

## Function `toUpper`
``` motoko no-repl
func toUpper<Ok, Err>(self : Result<Ok, Err>) : {#Ok : Ok; #Err : Err}
```

Converts a lower cased `#ok`, `#err` result type into an upper cased `#Ok`, `#Err` result type.
On the IC, a common convention is to use `#Ok` and `#Err` as the variants of a result type,
but in Motoko, we use `#ok` and `#err` instead.

Example:
```motoko include=import
let lower = #ok(42);
let upper = Result.toUpper(lower);
assert upper == #Ok(42);
```
