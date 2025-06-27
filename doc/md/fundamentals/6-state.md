---
sidebar_position: 6
hide_table_of_contents: true
---

# Mutable state

In Motoko, each actor can use internal mutable state but cannot share it directly with other actors. Immutable data, however, can be shared among actors and accessed via their external entry points, which act as shareable functions.

Mutable state is private to the actor that owns it and can be modified internally. In contrast, immutable values cannot be changed after creation and can be safely shared between actors.

For example, the following actor maintains a private mutable counter that can only be modified through its public API, in particular this ensures the counter can never decrease:

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

Since `count` is mutable, it can be modified internally but cannot be accessed directly from outside the actor. Instead, other actors must use its public interface to retrieve or update its value.

## Assignment to mutable memory

Mutable variables can be updated and immutable variables cannot.

You can update the value of mutable variables using the assignment syntax `:=`:

``` motoko no-repl
var count = 0;
count := 1
```

``` motoko no-repl
let count = 0;
count := 1 // This will result in a type error because you cannot reassign the value
```

### Special assignment operations

The assignment operation `:=` is general and works for all types.

Motoko provides special assignment operators that combine assignment with a binary operation. These operators update a variable by applying the operation between its current value and a given operand.

For example, numbers permit a combination of assignment and addition:

``` motoko
var count = 2;
count += 40;
```

After the second line, the variable `count` holds `42`.

Motoko includes other compound assignments as well, such as `#=`:

``` motoko
var text = "Motoko";
text #= " Ghost"
```

As with `+=`, this combined form avoids repeating the assigned variable’s name on the right hand side of the special assignment operator `#=`.

