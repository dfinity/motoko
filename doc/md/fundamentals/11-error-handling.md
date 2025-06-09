---
sidebar_position: 11
---

# Error handling

Using `Option` or `Result` is the preferred way of signaling errors in Motoko. They work in both synchronous and asynchronous contexts and make your APIs safer to use by encouraging clients to consider the error cases as well as the success cases. Exceptions should only be used to signal unexpected error states.

In addition to explicit error handling, Motoko provides traps and assertions for dealing with execution errors.

## Error reporting with `Option` types

When a function might either return a value of type `A` or signal an error, it can return an option type `?A`. In this pattern, `null` is used to indicate an error or missing result, while `?value` wraps a successful outcome. 

In the following example, if the `markDone` function sometimes fails and returns a number of seconds on success, its return type would be `async ?Seconds`. This makes it clear to callers that the result may be absent and must be handled safely.


Function definition:

``` motoko no-repl file=../examples/todo-error.mo#L49-L58
```

Function callsite:

``` motoko no-repl file=../examples/todo-error.mo#L117-L126
```

The main drawback of using option types to signal errors is that all failures are represented by a single, non-descriptive `null` value. This means important information about why something failed is lost. As a result, the only message the program can show the user might be something vague like `"Something went wrong."`

For this reason, option types should only be used for errors when there's just one clear reason for failure, and it can be easily understood at the callsite.

## Error reporting with `Result` types

While options are a built-in type, the `Result` is defined as a variant type like so:

``` motoko no-repl
type Result<Ok, Err> = { #ok : Ok; #err : Err }
```

Unlike option types, the Result type includes a second type parameter `Err` which allows you to specify exactly what kind of error occurred. This makes error handling more informative and flexible.

``` motoko no-repl file=../examples/todo-error.mo#L60-L60
```

The previous example can be revised to use `Result` types:

Function definition:

``` motoko no-repl file=../examples/todo-error.mo#L62-L76
```

Function callsite:

``` motoko no-repl file=../examples/todo-error.mo#L128-L141
```

## Pattern matching

The most common way of working with `Option` and `Result` is to use pattern matching. If you have a value of type `?Text`, you can use the `switch` keyword to access the potential [`Text`](./base/Text.md) contents:

``` motoko no-repl file=./examples/error-examples.mo#L3-L10
```

Motoko does not let you access the optional value without also considering the case that it is missing.

With a `Result` type, you can use pattern matching to handle both success and error cases. Unlike option types, the `#err` case carries detailed information about what went wrong, not just a `null` value.


``` motoko no-repl file=./examples/error-examples.mo#L12-L19
```

Sometimes you need to convert between `Option` and `Result` types. For example, a HashMap lookup returns `null` on failure (an `Option`), but if the caller has more context, they can turn that failure into a meaningful `Result` with an error message. On the other hand, sometimes you don’t need the extra detail from a `Result` and just want to convert any error (`#err`) into `null`.

The [base](https://github.com/dfinity/motoko-base) library provides `fromOption` and `toOption` functions in the `Result` module that make converting between these two types easy.

## Error reporting with `Error` (asynchronous errors)

Another way to handle errors in Motoko is with asynchronous `Error` handling, which is a limited form of exception handling. These errors can only be thrown and caught in asynchronous contexts, like inside `shared` functions or `async` blocks. Regular (non-`shared`) functions can’t use this kind of structured error handling.

This means you can `throw` an `Error` to exit a shared function early, and callers can `try` to run that code and `catch` the error if it happens. However, you can only use `throw` and `catch` inside asynchronous contexts.

Asynchronous `Error`s are best reserved for unexpected failures that you don’t expect most callers to handle. If you want the caller to handle possible failures explicitly, it’s better to use `Result` in your function’s return type.

Here’s how the `markDone` function might look using exceptions:

Function definition:

``` motoko no-repl file=../examples/todo-error.mo#L78-L92
```

Function callsite:

``` motoko no-repl file=../examples/todo-error.mo#L143-L150
```

## Traps

Traps immediately stop execution and roll back [state](https://internetcomputer.org/docs/motoko/fundamentals/state). They are used for fatal errors that cannot be recovered.

```motoko no-repl
import Debug "mo:base/Debug";

func divide(a : Nat, b : Nat) : Nat {
    if (b == 0) {
        Debug.trap("Cannot divide by zero");
    };
    return a / b;
};
```

## Assertions

Assertions enforce expected conditions. If the condition is false, they introduce a trap but are not errors themselves.

```motoko no-repl
func validateAge(age : Nat) : () {
    assert(age >= 18);  // Traps if age is below 18
};
```

## How not to handle errors

Using sentinel values to report errors is generally a bad practice and strongly discouraged. For example, you might have `markDone` return `-1` to indicate failure. In that case, the caller has to remember to check for that special value every time. It’s easy to forget, which can cause errors to go unnoticed or be detected too late.

## Error code reference

| Error Code | Description | Example | Explanation |
|------------|-------------------|---------|-------------|
| M0001 | Parsing errors. | `let x = 5 + ;` | Syntax error - missing operand after the `+` operator. |
| M0003 | Module tried to import itself. | `import Self "self.mo"` | A module cannot import itself, creating a circular dependency. |
| M0009 | File not found for import. | `import Lib "wrong/path_to_file.mo"` | The specified file path in the import statement doesn't exist. |
| M0010 | Imported package was not defined. | `import { undefined_module } from "mo:package"` | The imported module doesn't exist in the specified package. |
| M0014 | Non-static expression in library or module. | ```module { let x = Random.blob(); }``` | Modules can only contain static definitions, not runtime expressions. |
| M0029 | Unbound type. | `func transform(x : UnknownType) { ... }` | The type `UnknownType` is referenced but hasn't been defined. |
| M0030 | Type field does not exist in type. | ```type Person = { name : Text }; func getName(p : Person) : Nat { p.age }``` | The field `age` doesn't exist in the `Person` type. |
| M0031 | Shared function has non-shared parameter type. | ```public shared func process(obj : { var count : Nat }) { ... }``` | Shared functions cannot have mutable types as parameters. |
| M0032 | Shared function has non-shared return type. | ```public shared func getData() : { var data : [Nat] } { ... }``` | Shared functions cannot return mutable types. |
| M0033 | Async has non-shared content type. | ```async { var counter = 0 }``` | Async blocks cannot contain mutable [state](https://internetcomputer.org/docs/motoko/fundamentals/state). |
| M0036 | Invalid return type for shared query function. | ```public shared query func getUsers() : async [User] { ... }``` | Query functions cannot return async types. |
| M0038 | Misplaced await. | ```func compute() { let x = await promise; }``` | `await` can only be used in an async function or block. |
| M0045 | Wrong number of type arguments. | ```func process<T>(x : Array<T, Nat>) { ... }``` | `Array` type expects one type parameter, not two. |
| M0047 | Send capability required. | ```actor { public func send() { IC.call(...); } }``` | Functions calling IC management require explicit capability declarations. |
| M0050 | Literal does not have expected type. | ```let x : Text = 42;``` | Cannot assign a number literal to a variable of type [`Text`](https://internetcomputer.org/docs/motoko/base/Text). |
| M0055 | Cannot infer type of forward variable. | ```func process() { x := 10; var x = 0; }``` | Variable `x` is used before its declaration. |
| M0057 | Unbound variable. | ```func calculate() { return result; }``` | The variable `result` is used but hasn't been defined. |
| M0060 | Operator is not defined for operand types. | ```let result = "text" - 5;``` | The subtraction operator isn't defined for [`Text`](https://internetcomputer.org/docs/motoko/base/Text) and [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) types. |
| M0064 | Misplaced '!' without enclosing do block | ```func test() { someFunction!(); }``` | The `!` operator must be used within a `do` block. |
| M0070 | Expected object type. | ```let name = person.name;``` (where `person` is not an object) | Trying to access a field on a non-object type. |
| M0072 | Field does not exist in type. | ```type User = { id : Nat }; func getName(u : User) { u.name }``` | The field `name` doesn't exist in the `User` type. |
| M0073 | Expected mutable assignment target. | ```let x = 5; x := 10;``` | Cannot reassign to an immutable variable declared with `let`. |
| M0082 | Expected iterable type. | ```for (item in 42) { ... }``` | The value `42` is not an iterable type for a for-loop. |
| M0088 | Expected async type. | ```let result = await 42;``` | Cannot `await` on a non-async value. |
| M0089 | Redundant ignore. | ```ignore (5);``` | The `ignore` is unnecessary for a value that doesn't need to be ignored. |
| M0090 | Actor reference must have an [actor](https://internetcomputer.org/docs/motoko/fundamentals/actors-async) type .| ```let a = actor { ... }; a.someMethod();``` | The variable `a` must have an explicit [actor](https://internetcomputer.org/docs/motoko/fundamentals/actors-async) type to call its methods. |
| M0096 | Expression can't produce expected type. | ```func getValue() : Bool { return "true"; }``` | String cannot be returned where a [`Bool`](https://internetcomputer.org/docs/motoko/base/Bool) is expected. |
| M0097 | Expected function type. | ```let x = 5; x();``` | Cannot call a non-function value. |
| M0098 | Cannot instantiate function type. | ```type Func = (Nat) -> Nat; let f = Func();``` | Function types cannot be instantiated directly. |
| M0112 | Tuple pattern cannot consume type. | ```let (x, y) = { a = 1; b = 2 };``` | Cannot destructure an object using tuple pattern. |
| M0116 | Variant pattern cannot consume type. | ```switch (value) { case (#ok(x)) { ... } };``` (where `value` is not a variant) | Cannot match a non-variant value with variant patterns. |
| M0126 | Shared function cannot be private. | ```actor { private shared func publicAPI() { ... } }``` | Shared functions must be public in actors. |
| M0137 | A type or class was declared that explicitly or implicitly references an outer type parameter. | ```class Container<T>() { class Inner() { var value : T = ... } }``` | Inner classes cannot reference type parameters from outer classes. |
| M0139 | Inner actor classes are not supported. | ```class Outer() { actor class Inner() { ... } }``` | Actor classes cannot be nested inside other classes. |
| M0141 | Forbidden declaration in program. | ```import Prim "mo:prim";``` | Certain imports/declarations are not allowed in normal user code. |
| M0145 | Pattern does not cover value. | ```switch (option) { case (null) { ... } };``` (missing `?some` case) | Switch statement doesn't handle all possible values. |
| M0149 | An immutable [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) field (declared without `var`) was supplied where a mutable [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) field (specified with `var`), was expected. | ```type Mutable = { var x : Nat }; func set(r : Mutable) { ... }; set({ x = 10 });``` | Immutable [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) provided where mutable was expected. |
| M0150 | A mutable [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) field (declared with `var`) was supplied where an immutable [record](https://internetcomputer.org/docs/motoko/fundamentals/types/records) field (specified without `var`) was expected. | ```type Immutable = { x : Nat }; let record : Immutable = { var x = 10 };``` | Mutable field provided where immutable was expected. |
| M0151 | A object literal is missing some fields. | ```type Person = { name : Text; age : Nat }; let p : Person = { name = "John" };``` | The `age` field is missing from the object literal. |
| M0153 | An imported Candid file (.did) mentions types that cannot be represented in Motoko. | ```import Types from "types.did";``` (where `types.did` contains incompatible types) | The imported Candid file contains types that don't map to Motoko types. |
| M0154 | Deprecation annotation. | ```@deprecated("Use newFunction instead") func oldFunction() { ... }``` | Using a deprecated function or feature that has been marked with `@deprecated`. |
| M0155 | Inferred type [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) for subtraction. | ```let x = 5 - 10;``` | Subtraction with natural numbers can underflow without type annotation. |
| M0156 | A parameterized type definition, or set of type definitions, is too complicated for Motoko to accept. | ```type Complex<A,B,C,D,E,F> = ... (extremely nested recursive types)``` | Type definition too complex for the compiler to process. |
| M0157 | A type definition, or set of type definitions, is ill-defined. | ```type RecursiveType = RecursiveType;``` | A type directly references itself without proper indirection. |
| M0158 | A public class was declared without providing it with a name. | ```public class<T>() { ... }``` | Public parameterized classes must be named explicitly. |
| M0194 | An identifier was defined without referencing it later. | ```func process() { let unused = computeValue(); }``` | The variable `unused` is defined but never used. |
| M0195 | A function that demands elevated (system) capabilities was called without manifestly passing the capability. | ```func splitCycles() { let amount = ExperimentalCycles.balance() / 2; ExperimentalCycles.add(amount);}}``` | System capability function called without proper capability passing. |
| M0197 | A function that requires (system) capabilities was called in a context that does not provide them. | ```func invalidCycleAddition(): () { ExperimentalCycles.add(1000);}``` | Calling a function requiring system capabilities from a context without them. |
| M0198 | A field identifier was specified in an object pattern without referencing this identifier later. | ```func process(obj) { let { id } = obj; }``` | The extracted `id` field is never used in the function body. |

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />