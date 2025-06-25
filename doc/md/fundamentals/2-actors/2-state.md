---
sidebar_position: 2
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


<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
