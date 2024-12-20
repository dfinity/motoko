---
sidebar_position: 17
---

# Mutable state



Each actor in Motoko may use, but may never directly share, internal mutable state.

Immutable data can be [shared among actors](sharing.md), and also handled through each other's external entry points which serve as shareable functions. Unlike shareable data, a key Motoko design invariant is that mutable data is kept private to the actor that allocates it and is never shared remotely.

## Immutable vs mutable variables

The `var` syntax declares mutable variables in a declaration block:

``` motoko name=init
let text  : Text = "abc";
let num  : Nat = 30;

var pair : (Text, Nat) = (text, num);
var text2 : Text = text;
```

The declaration list above declares four variables. The first two variables (`text` and `num`) are lexically-scoped, immutable variables. The final two variables (`pair` and `text2`) are lexically-scoped, mutable variables.

## Assignment to mutable memory

Mutable variables permit assignment and immutable variables do not.

If you try to assign new values to either [`Text`](../base/Text.md) or `num` above, you will get static type errors because these variables are immutable.

You may freely update the value of mutable variables `pair` and `text2` using the syntax for assignment, written as `:=`, as follows:

``` motoko no-repl
text2 := text2 # "xyz";
pair := (text2, pair.1);
pair
```

In the example above, each variable is updated based on applying a simple update rule to their current values. Likewise, an actor processes some calls by performing updates on its private mutable variables, using the same assignment syntax as above.

### Special assignment operations

The assignment operation `:=` is general and works for all types.

Motoko includes special assignment operations that combine assignment with a binary operation. The assigned value uses the binary operation on a given operand and the current contents of the assigned variable.

For example, numbers permit a combination of assignment and addition:

``` motoko
var num2 = 2;
num2 += 40;
num2
```

After the second line, the variable `num2` holds `42`, as one would expect.

Motoko includes other combinations as well. For example, we can rewrite the line above that updates `text2` more concisely as:

``` motoko no-repl
text2 #= "xyz";
text2
```

As with `+=`, this combined form avoids repeating the assigned variable’s name on the right hand side of the special assignment operator `#=`.

The full table of [assignment operators](../reference/language-manual#assignment-operators) lists numerical, logical, and textual operations over appropriate types number, boolean and text values, respectively.

## Reading from mutable memory

Once you have updated each variable, you must read from the mutable contents. This does not require a special syntax.

Each use of a mutable variable looks like the use of an immutable variable, but does not act like one. In fact, its meaning is more complex. As in many other language, the syntax of each use hides the memory effect that accesses the memory cell identified by that variable and gets its current value. Other languages from functional traditions generally expose these effects syntactically.

## `var` vs `let` bound variables

Consider the following two variable declarations, which look similar:

``` motoko
let x : Nat = 0
```

``` motoko
var x : Nat = 0
```

The only difference in their syntax is the use of keyword `let` versus `var` to define the variable `x`, which in each case the program initializes to `0`.

However, these programs carry different meanings, and in the context of larger programs, the difference in meanings will impact the meaning of each occurrence of `x`.

For the first program, which uses `let`, each such occurrence means `0`. Replacing each occurrence with `0` will not change the meaning of the program.

For the second program, which uses `var`, each occurrence means “read and produce the current value of the mutable memory cell named `x`.” In this case, each occurrence’s value is determined by the dynamic state of the contents of the mutable memory cell named `x`.

As one can see from the definitions above, there is a fundamental contrast between the meanings of `let`-bound and `var`-bound variables.

In large programs, both kinds of variables can be useful, and neither kind serves as a good replacement for the other. However, `let`-bound variables are more fundamental.

For instance, instead of declaring `x` as a mutable variable initially holding `0`, you could instead use `y`, an immutable variable that denotes a mutable array with one entry holding `0`:

``` motoko
var x : Nat       = 0 ;
let y : [var Nat] = [var 0] ;
```

The read and write syntax required for this encoding reuses that of mutable arrays, which is not as readable as that of `var`-bound variables. As such, the reads and writes of variable `x` will be easier to read than those of variable `y`.

For this practical reason and others, `var`-bound variables are a core aspect of the language's design.

## Immutable arrays

Before discussing [mutable arrays](#mutable-arrays), we introduce immutable arrays, which share the same projection syntax but do not permit mutable updates after allocation.

### Allocate an immutable array of constants

``` motoko
let a : [Nat] = [1, 2, 3] ;
```

The array `a` above holds three natural numbers, and has type `[Nat]`. In general, the type of an immutable array is `[_]`, using square brackets around the type of the array’s elements, which must share a single common type.

### Read from an array index

You can read from an array using the usual bracket syntax of `[` and `]` around the index you want to access:

``` motoko no-repl
let x : Nat = a[2] + a[0] ;
```

Every array access in Motoko is safe. Accesses that are out of bounds will not access memory unsafely, but instead will cause the program to trap as with an [assertion](../getting-started/basic-concepts#traps) failure.

## The [`Array`](../base/Array.md) module

The Motoko standard library provides basic operations for immutable and mutable arrays. It can be imported as follows:

``` motoko no-repl
import Array "mo:base/Array";
```

For more information about using arrays, see the [array](../base/Array.md) library descriptions.

### Allocate an immutable array with varying content

Each new array allocated by a program will contain a varying number of elements. Without mutation, you need a way to specify this family of elements all at once in the argument to allocation.

To accommodate this need, the Motoko language provides the higher-order array allocation function `Array.tabulate`, which allocates a new array by consulting a user-provided generation function, `gen`,for each element.

``` motoko no-repl
func tabulate<T>(size : Nat,  gen : Nat -> T) : [T]
```

Function `gen` specifies the array as a function value of arrow type `Nat → T`, where `T` is the final array element type.

The function `gen` actually functions as the array during its initialization. It receives the index of the array element and produces the element of type `T` that should reside at that index in the array. The allocated output array populates itself based on this specification.

For instance, you can first allocate `array1` consisting of some initial constants, then functionally update some of the indices by changing them in a pure, functional way, to produce `array2`, a second array that does not destroy the first.

``` motoko no-repl
let array1 : [Nat] = [1, 2, 3, 4, 6, 7, 8] ;

let array2 : [Nat] = Array.tabulate<Nat>(7, func(i:Nat) : Nat {
    if ( i == 2 or i == 5 ) { array1[i] * i } // change 3rd and 6th entries
    else { array1[i] } // no change to other entries
  }) ;
```

Even though we changed `array1` into `array2` in a functional sense, notice that both arrays and both variables are immutable.

## Mutable arrays

Each mutable array in Motoko introduces private mutable actor state.

Because Motoko’s type system enforces that remote actors do not share their mutable state, the Motoko type system introduces a firm distinction between mutable and immutable arrays that impacts typing, subtyping, and the language abstractions for asynchronous communication.

Locally, the mutable arrays can not be used in places that expect immutable ones, since Motoko’s definition of [subtyping](../reference/language-manual#subtyping) for arrays correctly distinguishes those cases for the purposes of type soundness. Additionally, in terms of actor communication, immutable arrays are safe to send and share, while mutable arrays can not be shared or otherwise sent in messages. Unlike immutable arrays, mutable arrays have non-shareable types.

### Allocate a mutable array of constants

To indicate allocation of mutable arrays, the mutable array syntax `[var _]` uses the `var` keyword in both the expression and type forms:

``` motoko
let a : [var Nat] = [var 1, 2, 3] ;
```

As above, the array `a` above holds three natural numbers, but has type `[var Nat]`.

### Allocate a mutable array with dynamic size

To allocate mutable arrays of non-constant size, use the `Array.init` base library function and supply an initial value:

``` motoko no-repl
func init<T>(size : Nat,  x : T) : [var T]
```

For example:

``` motoko no-repl
var size : Nat = 42 ;
let x : [var Nat] = Array.init<Nat>(size, 3);
```

The variable `size` does not need to be constant here. The array will have `size` number of entries, each holding the initial value `3`.

### Mutable updates

Mutable arrays, each with type form `[var _]`, permit mutable updates via assignment to an individual element. In this case, element index `2` gets updated from holding `3` to instead hold value `42`:

``` motoko
let a : [var Nat] = [var 1, 2, 3];
a[2] := 42;
a
```

### Subtyping does not permit mutable to be used as immutable

Subtyping in Motoko does not permit us to use a mutable array of type `[var Nat]` in places that expect an immutable one of type `[Nat]`.

There are two reasons for this. First, as with all mutable state, mutable arrays require different rules for sound subtyping. In particular, mutable arrays have a less flexible subtyping definition, necessarily. Second, Motoko forbids uses of mutable arrays across [asynchronous communication](actors-async.md), where mutable state is never shared.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />