---
sidebar_position: 8
---
# Functions

## Query functions

Query functions are designed for retrieving data. Query functions cannot modify state. They execute faster than update functions because they do not require consensus.

```motoko no-repl
actor Counter {
    stable var count: Nat = 0;
    public query func getCount(): async Nat {
        return count;
    };
};
```

### Composite queries

Composite queries chain multiple query calls within the same function.

```motoko no-repl
import Nat "mo:base/Nat";
actor Counter {
  stable var count : Nat = 0;
  public query func getCount() : async Nat {
    return count
  };
  public composite query func isEven() : async Bool {
    return (await getCount()) % 2 == 0
  };
  public composite query func summary() : async Text {
    let current = await getCount();
    let even = await isEven();
    if (even) {
      return "Count is " # Nat.toText(current) # " and it is even."
    } else {
      return "Count is " # Nat.toText(current) # " and it is odd."
    }
  };
}
```

## Arguments

An argument is a value passed to a function when called. Arguments enable functions to process incoming data during execution.

### Single argument

A function can take a single argument of a specific type:

```motoko no-repl
public func increment(amount: Nat) : async Nat {
  count += amount;
  return count;
}
```

### Multiple arguments

Functions can accept multiple arguments by separating them with commas:

```motoko no-repl
public func add(x: Nat, y: Nat) : async Nat {
  return x + y;
}
```

### Record as an argument

Multiple values can be passed as a single argument by encapsulating them within a record type:

```motoko no-repl
type UserData = { name: Text; age: Nat };
public func createUser(userData: UserData) : async Text {
  return userData.name;
}
```

### Array as an argument

A collection of values can be passed as a single array argument:

```motoko no-repl
public func sum(numbers: [Nat]) : async Nat {
  var total : Nat = 0;
  for (num in numbers.vals()) { total += num };
  return total;
}
```
