---
sidebar_position: 6
hide_table_of_contents: true
---

# Mutable state

<!----This page's content was dramatically shorter than the old docs page. I've ported over most of the old content and made editorial revisions.--->

In Motoko, each actor can use internal mutable state but cannot share it directly with other actors. Immutable data, however, can be shared among actors and accessed via their external entry points, which act as shareable functions.

Mutable state is private to the actor that owns it and can be modified internally. This local confinement helps prevent race conditions. In contrast, immutable state cannot be changed after creation and can be safely shared between actors. Because of its unchangeable nature, immutable state is safe to pass around in remote calls, supporting concurrency across actors without risk of conflicts.

Motoko enforces strict control over mutable state, ensuring that concurrent execution remains predictable and error-free.

For example, following actor maintains a private mutable counter that can only be modified through its own functions:

```motoko no-repl
actor Counter {
    stable var count : Nat = 0; // Private mutable state

    public shared func increment() : async Nat {
        count += 1;
        return count;
    };

    public query func getCount() : async Nat {
        return count;
    };
};
```

Since `count` is mutable, it can be modified internally but cannot be accessed directly from outside the actor. Instead, other actors must use the provided entry points to retrieve or update its value.

## Assignment to mutable memory

Mutable variables permit assignment and immutable variables do not.

You may freely update the value of mutable variables `pair` and `text2` using the syntax for assignment, written as `:=`, as follows:

``` motoko no-repl
text2 := text2 # "xyz";
pair := (text2, pair.1);
pair
```

In the example above, each variable is updated based on applying a simple update rule to their current values. Likewise, an actor processes some calls by performing updates on its private mutable variables, using the same assignment syntax as above.

### Special assignment operations

The assignment operation `:=` is general and works for all types.

Motoko provides special assignment operators that combine assignment with a binary operation. These operators update a variable by applying the operation between its current value and a given operand.

For example, numbers permit a combination of assignment and addition:

``` motoko
var num2 = 2;
num2 += 40;
num2
```

After the second line, the variable `num2` holds `42`.

Motoko includes other combinations as well. For example, you can rewrite the line above that updates `text2` more concisely as:

``` motoko no-repl
text2 #= "xyz";
text2
```

As with `+=`, this combined form avoids repeating the assigned variable’s name on the right hand side of the special assignment operator `#=`.

## Reading from mutable memory

After updating mutable variables, you can read their values without any special syntax. Using a mutable variable looks the same as using an immutable one, but its behavior is more complex. Each use actually accesses the memory cell tied to that variable to get its current value, though this memory effect is hidden in the syntax. In contrast, many functional languages make these effects explicit in their syntax.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />