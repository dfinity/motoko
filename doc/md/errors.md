# Errors and Options

There are three primary ways to represent and handle errors values in Motoko:

-   Option values (with a non-informative `null` indicated *some* error);

-   `Result` variants (with a descriptive `#err value` providing more information about the error); and

-   `Error` values (that, in an asynchronous context, can be thrown and caught - similar to exceptions - and contain a numeric code and message).

## Our Example API

Let’s assume we’re building an API for a Todo application and want to expose a function that lets a user mark one of their Todo’s as **Done**. To keep it simple we’ll accept a `TodoId` and return an `Int` that represents how many seconds the Todo has been open. We’re also assuming we’re running in our own actor so we return an async value. If nothing would ever go wrong that would leave us with the following API:

``` motoko no-repl
func markDone(id : TodoId) : async Int
```

The full definition of all types and helpers we’ll use in this document is included for reference:

``` motoko no-repl file=./examples/todo-error.mo#L1-L6
```

``` motoko no-repl file=./examples/todo-error.mo#L10-L37
```

## When things go wrong

We now realize that there are conditions under which marking a Todo as done fails.

-   The `id` could reference a non-existing Todo

-   The Todo might already be marked as done

We’ll now talk about the different ways to communicate these errors in Motoko and slowly improve our solution.

## What error type to prefer

### How *not* to do things

One particularly easy and *bad* way of reporting errors is through the use of a *sentinel* value. For example, for our `markDone` function we might decide to use the value `-1` to signal that something failed. The callsite then has to check the return value against this special value and report the error. But it’s way too easy to not check for that error condition and continue to work with that value in our code. This can lead to delayed or even missing error detection and is strongly discouraged.

Definition:

``` motoko no-repl file=./examples/todo-error.mo#L38-L47
```

Callsite:

``` motoko no-repl file=./examples/todo-error.mo#L108-L115
```

### Prefer Option/Result over Exceptions where possible

Using `Option` or `Result` is the preferred way of signaling errors in Motoko. They work in both synchronous and asynchronous contexts and make your APIs safer to use (by encouraging clients to consider the error cases as well as the success cases. Exceptions should only be used to signal unexpected error states.

### Error reporting with Option

A function that wants to return a value of type `A` or signal an error can return a value of *option* type `?A` and use the `null` value to designate the error. In our example this means having our `markDone` function return an `async ?Seconds`.

Here’s what that looks like for our `markDone` function:

Definition:

``` motoko no-repl file=./examples/todo-error.mo#L49-L58
```

Callsite:

``` motoko no-repl file=./examples/todo-error.mo#L117-L126
```

The main drawback of this approach is that it conflates all possible errors with a single, non-informative `null` value. Our callsite might be interested in why marking a `Todo` as done has failed, but that information is lost by then, which means we can only tell the user that `"Something went wrong."`. Returning option values to signal errors should only be used if there just one possible reason for the failure, and that reason can be easily determined at the callsite. One example of a good usecase for this is a HashMap lookup failing.

### Error reporting with `Result` types

To address the shortcomings of using option types to signal errors we’ll now look at the richer `Result` type. While options are a built-in type, the `Result` is defined as a variant type like so:

``` motoko no-repl
type Result<Ok, Err> = { #ok : Ok; #err : Err }
```

Because of the second type parameter, `Err`, the `Result` type lets us select the type we use to describe errors. So we’ll define a `TodoError` type our `markDone` function will use to signal errors.

``` motoko no-repl file=./examples/todo-error.mo#L60-L60
```

This lets us now write the third version of `markDone`:

Definition:

``` motoko no-repl file=./examples/todo-error.mo#L62-L76
```

Callsite:

``` motoko no-repl file=./examples/todo-error.mo#L128-L141
```

And as we can see we can now give the user a useful error message.

## Working with Option/Result

`Option`s and `Results`s are a different way of thinking about errors, especially if you come from a language with pervasive exceptions. In this chapter we’ll look at the different ways to create, destructure, convert, and combine `Option`s and `Results` in different ways.

### Pattern matching

The first and most common way of working with `Option` and `Result` is to use 'pattern matching'. If we have a value of type `?Text` we can use the `switch` keyword to access the potential `Text` contents:

``` motoko no-repl file=./examples/error-examples.mo#L3-L10
```

The important thing to understand here is that Motoko does not let you access the optional value without also considering the case that it is missing.

In the case of a `Result` we can also use pattern matching, with the difference that we also get an informative value (not just `null`) in the `#err` case.

``` motoko no-repl file=./examples/error-examples.mo#L12-L19
```

### Higher-Order functions

Pattern matching can become tedious and verbose, especially when dealing with multiple optional values. The [base](https://github.com/dfinity/motoko-base) library exposes a collection of higher-order functions from the `Optional` and `Result` modules to improve the ergonomics of error handling.

### Converting back and forth between Option/Result

Sometimes you’ll want to move between Options and Results. A Hashmap lookup returns `null` on failure and that’s fine, but maybe the caller has more context and can turn that lookup failure into a meaningful `Result`. At other times you don’t need the additional information a `Result` provides and just want to convert all `#err` cases into `null`. For these situations [base](https://github.com/dfinity/motoko-base) provides the `fromOption` and `toOption` functions in the `Result` module.

## Asynchronous Errors

The last way of dealing with errors in Motoko is to use asynchronous `Error` handling, a restricted form of the exception handling familiar from other languages. Unlike the exceptions of other languages, Motoko *errors* values, can only be thrown and caught in asynchronous contexts, typically the body of a `shared` function or `async` expression. Non-`shared` functions cannot employ structured error handling. This means you can exit a shared function by `throw`ing an `Error` value and `try` some code calling a shared function on another actor, `catch`ing its failure as a result of type `Error`, but you can’t use these error handling constructs in regular code, outside of an asynchronous context.

Asynchronous `Error`s should generally only be used to signal unexpected failures that you cannot recover from, and that you don’t expect many consumers of your API to handle. If a failure should be handled by your caller you should make it explicit in your signature by returning a `Result` instead. For completeness here is the `markDone` example with exceptions:

Definition:

``` motoko no-repl file=./examples/todo-error.mo#L78-L92
```

Callsite:

``` motoko no-repl file=./examples/todo-error.mo#L143-L150
```
