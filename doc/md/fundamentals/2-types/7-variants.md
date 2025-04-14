---
sidebar_position: 7
---

# Variants

Variants allow defining values that can take on multiple possible forms, each labeled with a distinct tag. Unlike [records](https://internetcomputer.org/docs/motoko/fundamentals/types/records), where all fields exist at once, a variant holds exactly one of its possible values at any given time. This makes variants useful for representing states, actions, or categorized data.


## Defining a variant

```motoko no-repl
type Status = {
    #Active;
    #Inactive;
    #Banned : Text;
};
```

`#Active` and `#Inactive` are unit tags, meaning they do not store any extra data `()`. `#Banned` carries a [`Text`](https://internetcomputer.org/docs/motoko/base/Text) value, such as the reason for the ban.

## Assigning variants

To assign a variant value, use one of the defined tags.

```motoko no-repl
let activeUser: Status = #Active;
let bannedUser: Status = #Banned("Violation of rules");
```

## Accessing a variant's value

To work with a variant, use a [`switch`](https://internetcomputer.org/docs/motoko/fundamentals/control-flow/switch) expression to match each possible case.

```motoko no-repl
let activeUser: Status = #Active;
let bannedUser: Status = #Banned("Violation of rules");

func getStatusMessage(status: Status) : Text {
    switch (status) {
        case (#Active) "User is active";
        case (#Inactive) "User is inactive";
        case (#Banned(reason)) "User is banned: " # reason;
    }
}

Debug.print(getStatusMessage(activeUser));
Debug.print(getStatusMessage(bannedUser));
```

## Variants example: traffic lights

To demonstrate variants, consider the following example.

A traffic light cycles between three distinct states:

- Red: Vehicles must stop.
- Yellow: Vehicles should prepare to stop.
- Green: Vehicles may proceed.

Since the traffic light can only be in one of these states at a time, a variant is well-suited to model it. There is no invalid [state](https://internetcomputer.org/docs/motoko/fundamentals/state), as every possible value is explicitly defined. The transitions are controlled and predictable.

### Defining the traffic light state

```motoko no-repl
// Each tag #red, #yellow, #green represents one possible state of the traffic light.
type TrafficLight = {
    #red;
    #yellow;
    #green;
};
```

### Transitioning between states

A function can define how the traffic light cycles from one [state](https://internetcomputer.org/docs/motoko/fundamentals/state) to the next.

```motoko no-repl
func nextState(light: TrafficLight): TrafficLight {
    switch (light) {
        case (#red)    #green;
        case (#green)  #yellow;
        case (#yellow) #red;
    }
};
```

### Simulating traffic light changes

```motoko no-repl
 type TrafficLight = {
    #red;
    #yellow;
    #green
  };

  func nextState(light : TrafficLight) : TrafficLight {
    switch (light) {
      case (#red) #green;
      case (#green) #yellow;
      case (#yellow) #red
    }
  };

  var light : TrafficLight = #red; // Initial state

  for (_ in Iter.range(0, 5)) {
    // Cycle through states
    light := nextState(light);
    Debug.print(debug_show (light))
  };
```

#### Output

```bash
#green
#yellow
#red
#green
#yellow
#red
```

## Defining a binary tree type using variants

A binary tree is a data structure where each node has up to two child nodes. A variant can be used to represent this structure since a node can either contain a value with left and right children or be an empty leaf. This tree type is recursive as it refers to itself in its definition.

```motoko no-repl
type Tree = {
    #node : {
      value : Nat;
      left : Tree;
      right : Tree
    };
    #leaf
  };
```

This example contains two variants:

1. `#node` contains a value of type [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) and two child trees (`left` and `right`).
2. `#leaf` represents an empty node.

### Building the tree

The following example defines a tree with a single root node containing the value `10`. It has two child nodes, `5` and `15`, both of which do not have any children.

```motoko no-repl
let tree : Tree = #node {
    value = 10;
    left = #node {value = 5; left = #leaf; right = #leaf};
    right = #node {value = 15; left = #leaf; right = #leaf}
  };
```

### Tree structure

```md
        10
     /      \
    5       15
```

### Traversing the tree

A tree can be traversed in multiple ways. One common approach is in-order traversal, where nodes are visited in the order:

1. Left subtree
2. Root node
3. Right subtree

The following example recursively traverses the tree in order and prints each value as it is visited.

```motoko no-repl
type Tree = {
    #node : {
      value : Nat;
      left : Tree;
      right : Tree
    };
    #leaf
  };

let tree : Tree = #node {
    value = 10;
    left = #node {value = 5; left = #leaf; right = #leaf};
    right = #node {value = 15; left = #leaf; right = #leaf}
  };

func traverseInOrder(t : Tree) {
    switch (t) {
      case (#leaf) {};
      case (#node {value; left; right}) {
        traverseInOrder(left);
        Debug.print(debug_show (value));
        traverseInOrder(right)
      }
    }
  };
  traverseInOrder(tree);
```

### Using generic types

Currently, the example tree only supports [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) values. To allow it to store any type of data, a [generic type](https://internetcomputer.org/docs/motoko/fundamentals/types/advanced-types) can be used. A generic type allows a data structure to work with multiple types by using a placeholder type `T`, which is replaced with a specific type when used.

```motoko no-repl
type Tree<T> = {
    #node : {
      value : T;
      left : Tree<T>;
      right : Tree<T>;
    };
    #leaf;
};
```

With this change, the tree can store any type, such as [`Text`](https://internetcomputer.org/docs/motoko/base/Text), [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat), or custom types, making it more flexible and reusable.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />