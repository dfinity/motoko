---
sidebar_position: 5
---

# Variants

Variants allow defining values that can take on multiple possible forms, each labeled with a distinct tag. Unlike records, where all fields exist at once, a variant holds exactly one of its possible values at any given time. This makes variants useful for representing states, actions, or categorized data.

## Modeling traffic lights with variants

A traffic light cycles between three distinct states:

- Red – Vehicles must stop.
- Yellow – Vehicles should prepare to stop.
- Green – Vehicles may proceed.

Since the traffic light can only be in one of these states at a time, a variant is well-suited to model it. The traffic light cannot be in two states at the same time. There is no invalid state —every possible value is explicitly defined. The transitions are controlled and predictable.

### Defining the traffic light state

```motoko norepl
// Each tag #red, #yellow, #green represents one possible state of the traffic light.
type TrafficLight = {
    #red;
    #yellow;
    #green;
};
```

### Transitioning between states

A function can define how the traffic light cycles from one state to the next.

```motoko norepl
func nextState(light: TrafficLight): TrafficLight {
    switch (light) {
        case (#red)    #green;
        case (#green)  #yellow;
        case (#yellow) #red;
    }
};
```

### Simulating traffic light changes

```motoko norepl
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

``` md
#green
#yellow
#red
#green
#yellow
#red
```

## Defining a binary tree type using variants

A binary tree is a data structure where each node has up to two child nodes. A variant is well-suited to represent this structure since a node can either contain a value with left and right children or be an empty leaf. This tree type is recursive as it refers to itself in its definition.

```motoko
type Tree = {
    #node : {
      value : Nat;
      left : Tree;
      right : Tree
    };
    #leaf
  };
```

This contains two variants:

`#node` contains:

- A value of type `Nat`.
- Two child trees (`left` and `right`).
`#leaf` represents an empty node.

### Building the tree

The following example defines a tree with a single root node containing the value `10`. It has two child nodes, `5` and `15`, both of which do not have any children.

```motoko
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

The following recursively traverses the tree in order and prints each value as it is visited:  

```motoko
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

### Generalizing the tree with generics  

Currently, the tree only supports `Nat` values. To allow it to store any type of data, a generic type can be used. A generic type allows a data structure to work with multiple types by using a placeholder type `T`, which is replaced with a specific type when used.  

```motoko
type Tree<T> = {
    #node : {
      value : T;
      left : Tree<T>;
      right : Tree<T>;
    };
    #leaf;
};
```  

With this change, a tree can store any type, such as `Text`, `Nat`, or custom types, making it more flexible and reusable.