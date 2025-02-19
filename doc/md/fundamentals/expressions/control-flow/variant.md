# Variant

A variant is a type that can hold one of multiple possible values, each labeled with a distinct tag. Unlike records, which contain multiple fields at once, a variant only holds a single field at a time.

## Defining a variant

A variant type consists of tags, each representing a possible value. Each tag can have an associated type or be a unit tag (no associated value). Variants are especially useful for defining recursive data structures, such as a binary tree.

```motoko
type Status = {
    #Active;
    #Inactive;
    #Banned : Text;
};
```

`#Active` and `#Inactive` are unit tags, meaning they do not store any extra data `()`. `#Banned` carries a `Text` value, such as the reason for banning.
  
## Assigning variants

To assign a variant value, use one of the defined tags:

```motoko
let activeUser: Status = #Active;
let bannedUser: Status = #Banned("Violation of rules");
```

- `activeUser` is assigned `#Active`.
- `bannedUser` holds `#Banned("Violation of rules")`, carrying additional information.

## Accessing a variant's value

To work with a variant, use a `switch` expression to match each possible case:

```motoko
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

`switch` ensures every possible variant is handled safely. The `#Banned` case extracts the `reason` stored inside.

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

- `#node` contains:
  - A value of type `Nat`.
  - Two child trees (`left` and `right`).
- `#leaf` represents an empty node.

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

```
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
