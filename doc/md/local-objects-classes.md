# Local objects and classes

In Motoko, an `object` may encapsulate local state (`var`-bound variables) by packaging this state with `public` methods that access and update it.

As in other typed languages, Motoko programs benefit from the ability to encapsulate state as objects with abstract types.

However, Motoko objects that include mutable state are *not shareable*, and this is a critical security-oriented design decision.

If they were shareable, that would mean either conceptually moving a mobile object’s code among actors and executing it remotely, a security risk, or sharing state with remote logic, another security risk. (Notably, as a subcase, objects may be pure records and those *are* shareable, since they are free from mutable state.)

To compensate for this necessary limitation, `actor` objects *are shareable*, but always execute remotely. They communicate with shareable Motoko data only. Local objects interact in less restricted ways with themselves, and can pass any Motoko data to each other’s methods, including other objects. In most other ways, local objects (and classes) are non-shareable counterparts to actor objects (and classes).

The [Mutable state](mutable-state.md) introduced declarations of private mutable state, in the form of `var`-bound variables and (mutable) array allocation. In this chapter, we use mutable state to implement simple objects, much like how we would implement simple objects in object-oriented programming.

We illustrate this support via a running example, which continues in the next chapter. The following example illustrates a general evolution path for Motoko programs. Each *object*, if important enough, has the potential to be refactored into an Internet *service*, by refactoring this *(local) object* into an *actor object*.

**Object classes**. Frequently, one needs *a family* of related objects to perform a task. When objects exhibit similar behavior, it makes sense to fabricate them according to the same blueprint, but with customizable initial state. To this end, Motoko provides a syntactical construct, called a `class` definition, which simplifies building objects of the same type and implementation. We introduce these after discussing objects.

**Actor classes**. When an object class exposes a *[service](actors-async.md)* (asynchronous behavior), the corresponding Motoko construct is an [actor class](actors-async.md), which follows a similar (but distinct) design.

## Example: The `counter` object

Consider the following *object declaration* of the object value `counter`:

``` motoko
object counter {
  var count = 0;
  public func inc() { count += 1 };
  public func read() : Nat { count };
  public func bump() : Nat {
    inc();
    read()
  };
};
```

This declaration introduces a single object instance named `counter`, whose entire implementation is given above.

In this example, the developer exposes three *public* functions `inc`, `read` and `bump` using keyword `public` to declare each in the object body. The body of the object, like a block expression, consists of a list of declarations.

In addition to these three functions, the object has one (private) mutable variable `count`, which holds the current count, initially zero.

## Object types

This object `counter` has the following *object type*, written as a list of field-type pairs, enclosed in braces (`{` and `}`):

``` motoko no-repl
{
  inc  : () -> () ;
  read : () -> Nat ;
  bump : () -> Nat ;
}
```

Each field type consists of an identifier, a colon `:`, and a type for the field content. Here, each field is a function, and thus has an *arrow* type form (`_ → _`).

In the declaration of `object`, the variable `count` was explicitly declared neither as `public` nor as `private`.

By default, all declarations in an object block are `private`, as is `count` here. Consequently, the type for `count` does not appear in the type of the object, *and* its name and presence are both inaccessible from the outside.

The inaccessibility of this field comes with a powerful benefit: By not exposing this implementation detail, the object has a *more general* type (fewer fields), and as a result, is interchangeable with objects that implement the same counter object type differently, without using such a field.

## Example: The `byteCounter` object

To illustrate the point just above, consider this variation of the `counter` declaration above, of `byteCounter`:

``` motoko
import Nat8 "mo:base/Nat8";
object byteCounter {
  var count : Nat8 = 0;
  public func inc() { count += 1 };
  public func read() : Nat { Nat8.toNat(count) };
  public func bump() : Nat { inc(); read() };
};
```

This object has the same type as the previous one, and thus from the standpoint of type checking, this object is interchangeable with the prior one:

``` motoko no-repl
{
  inc  : () -> () ;
  read : () -> Nat ;
  bump : () -> Nat ;
}
```

Unlike the first version, however, this version does not use the same implementation of the counter field. Rather than use an ordinary natural `Nat` that never overflows, but may also grow without bound, this version uses a byte-sized natural number (type `Nat8`) whose size is always eight bits.

As such, the `inc` operation may fail with an overflow for this object, but never the prior one, which may instead (eventually) fill the program’s memory, a different kind of application failure.

Neither implementation of a counter comes without some complexity, but in this case, they share a common type.

In general, a common type shared among two implementations (of an object or service) affords the potential for the internal implementation complexity to be factored away from the rest of the application that uses it. Here, the common type abstracts over the simple choice of a number’s representation. In general, the implementation choices would each be more complex, and more interesting.

## Object subtyping

To illustrate the role and use of object subtyping in Motoko, consider implementing a simpler counter with a more general type (fewer public operations):

``` motoko
object bumpCounter {
  var c = 0;
  public func bump() : Nat {
    c += 1;
    c
  };
};
```

The object `bumpCounter` has the following object type, exposing exactly one operation, `bump`:

``` motoko no-repl
{
  bump : () -> Nat ;
 }
```

This type exposes the most common operation, and one that only permits certain behavior. For instance, the counter can only ever increase, and can never decrease or be set to an arbitrary value.

In other parts of a system, we may in fact implement and use a *less general* version, with *more* operations:

``` motoko no-repl
fullCounter : {
  inc   : () -> () ;
  read  : () -> Nat ;
  bump  : () -> Nat ;
  write : Nat -> () ;
}
```

Here, we consider a counter named `fullCounter` with a less general type than any given above. In addition to `inc`, `read` and `bump`, it additionally includes `write`, which permits the caller to change the current count value to an arbitrary one, such as back to `0`.

**Object subtyping.** In Motoko, objects have types that may relate by subtyping, as the various types of counters do above. As is standard, types with *more fields* are *less general* (are ***sub**types* of) types with *fewer fields*. For instance, we can summarize the types given in the examples above as being related in the following subtyping order:

-   Most general:

``` motoko no-repl
{ bump : () -> Nat }
```

-   Middle generality:

``` motoko no-repl
{
  inc  : () -> () ;
  read : () -> Nat ;
  bump : () -> Nat ;
}
```

-   Least generality:

``` motoko no-repl
{
  inc  : () -> () ;
  read : () -> Nat ;
  bump : () -> Nat ;
  write : Nat -> () ;
}
```

If a function expects to receive an object of the first type (`{ bump: () → Nat }`), *any* of the types given above will suffice, since they are each equal to, or a subtype of, this (most general) type.

However, if a function expects to receive an object of the last, least general type, the other two will *not* suffice, since they each lack the needed `write` operation, to which this function rightfully expects to have access.

## Object classes

In Motoko, an object encapsulates state, and an object `class` is a package of two entities that share a common name.

Consider this example `class` for counters that start at zero:

``` motoko name=counter
class Counter() {
  var c = 0;
  public func inc() : Nat {
    c += 1;
    return c;
  }
};
```

The value of this definition is that we can *construct* new counters, each starting with their own unique state, initially at zero:

``` motoko name=cinit include=counter
let c1 = Counter();
let c2 = Counter();
```

Each is independent:

``` motoko include=counter,cinit
let x = c1.inc();
let y = c2.inc();
(x, y)
```

We could achieve the same results by writing a function that returns an object:

``` motoko
func Counter() : { inc : () -> Nat } =
  object {
    var c = 0;
    public func inc() : Nat { c += 1; c }
  };
```

Notice the return type of this *constructor function* (an object type):

``` motoko no-repl
{ inc : () -> Nat }
```

We may want to name this type, for example, `Counter`, as follows, for use in further type declarations:

``` motoko no-repl
type Counter = { inc : () -> Nat };
```

In fact, the `class` keyword syntax shown above is nothing but a shorthand for these two definitions for `Counter`: a factory function `Counter` that constructs objects, and the type `Counter` of these objects. Classes do not provide any new functionality beyond this convenience.

### Class constructor

An object class defines a constructor function that may carry zero or more data arguments and zero or more type arguments.

The `Counter` example above has zero of each.

The type arguments, if any, parameterize both the type and the constructor function for the class.

The data arguments, if any, parameterize (only) the constructor function for the class.

#### Data arguments

Suppose we want to initialize the counter with some non-zero value. We can supply that value as a data argument to the `class` constructor:

``` motoko
class Counter(init : Nat) {
  var c = init;
  public func inc() : Nat { c += 1; c };
};
```

This parameter is available to all methods.

For instance, we can `reset` the `Counter` to its initial value, a parameter:

``` motoko
class Counter(init : Nat) {
  var c = init;
  public func inc() : Nat { c += 1; c };
  public func reset() { c := init };
};
```

#### Type arguments

Suppose we want the counter to actually carry data that it counts (like a specialized `Buffer`).

When classes use or contain data of arbitrary type, they carry a type argument (or equivalently, *type parameter*) for that unknown type, just as with functions.

The scope of this type parameter covers the entire `class`, just as with data parameters. As such, the methods of the class can use (and *need not re-introduce*) these type parameters.

``` motoko
import Buffer "mo:base/Buffer";

class Counter<X>(init : Buffer.Buffer<X>) {
  var buffer = init.clone();
  public func add(x : X) : Nat {
    buffer.add(x);
    buffer.size()
  };

  public func reset() {
    buffer := init.clone()
  };
};
```

#### Type annotation

Optionally, the class constructor may also carry a type annotation for its "return type" (the type of objects that it produces). When supplied, Motoko checks that this type annotation is compatible with the body of the class (an object definition). This check ensures that each object produced by the constructor meets the supplied specification.

For example, we repeat the `Counter` as a buffer, and annotate it with a more general type `Accum<X>` that permits adding, but not resetting the counter. This annotation ensures that the objects are compatible with the type `Accum<X>`.

``` motoko
import Buffer "mo:base/Buffer";

type Accum<X> = { add : X -> Nat };

class Counter<X>(init : Buffer.Buffer<X>) : Accum<X> {
  var buffer = init.clone();
  public func add(x : X) : Nat { buffer.add(x); buffer.size() };
  public func reset() { buffer := init.clone() };
};
```

#### Full syntax

In full, classes are defined by the keyword `class`, followed by: - a name for the constructor and type being defined (for example, `Counter`) - optional type arguments (for example, omitted, or `<X>`, or `<X, Y>`) - an argument list (for example, `()`, or `(init : Nat)`, etc.) - an optional type annotation for the constructed objects (for example, omitted, or `Accum<X>`), - the class "body" is an object definition, parameterized by the type and value arguments (if any).

The constituents of the body marked `public` contribute to the resulting objects' type and these types compared against the (optional) annotation, if given.

##### Another example: `Bits`

As another example, let’s consider the task of walking the bits of a natural number (type `Nat`). For this example, we could define the following:

``` motoko
class Bits(n : Nat) {
  var state = n;
  public func next() : ?Bool {
    if (state == 0) { return null };
    let prev = state;
    state /= 2;
    ?(state * 2 != prev)
  }
}
```

The above class definition is equivalent to the simultaneous definition of a structural type synonym and a factory function, both named `Bits`:

``` motoko
type Bits = {next : () -> ?Bool}
let Bits : Nat -> Bits =
func Bits(n : Nat) : Bits = object {
  // class body
};
```

## Structural subtyping

Object subtyping in Motoko uses *structural subtyping*, not *nominal subtyping*.

Recall that in nominal typing, the question of two types equality depends on choosing consistent, globally-unique type names (across projects and time).

In Motoko, the question of two types' equality is based on their *structure*, not their names.

Due to structural typing, naming the class type provides a convenient abbreviation.

For typing purposes, however, all that matters is the *structure* of the corresponding object type: two classes with different names but equivalent definitions produce type-compatible objects.

When the optional type annotation is supplied in a class declaration, conformance is checked: the object type must be a subtype of the annotation. The annotation does not affect the type of the class, however, even if it only describes a proper super-type of the object type.

Formally, subtyping relationships in Motoko extend to all types, not just object types.

Most cases are standard, and follow conventional programming language theory (for *structural* subtyping, specifically).

Other notable cases in Motoko for new programmers include array, options, variants and number type inter-relationships.
