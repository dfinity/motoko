# core/Types
Common types used throughout the core package.

Example usage:

```motoko name=import
import { type Result; type Iter } "mo:core/Types";

// Result for error handling
let result : Result<Int, Text> = #ok(42);

// Iterator for sequences
let iter : Iter<Nat> = { next = func() { ?1 } };
```

## Type `Blob`
``` motoko no-repl
type Blob = Prim.Types.Blob
```


## Type `Bool`
``` motoko no-repl
type Bool = Prim.Types.Bool
```


## Type `Char`
``` motoko no-repl
type Char = Prim.Types.Char
```


## Type `Error`
``` motoko no-repl
type Error = Prim.Types.Error
```


## Type `ErrorCode`
``` motoko no-repl
type ErrorCode = Prim.ErrorCode
```


## Type `Float`
``` motoko no-repl
type Float = Prim.Types.Float
```


## Type `Int`
``` motoko no-repl
type Int = Prim.Types.Int
```


## Type `Int8`
``` motoko no-repl
type Int8 = Prim.Types.Int8
```


## Type `Int16`
``` motoko no-repl
type Int16 = Prim.Types.Int16
```


## Type `Int32`
``` motoko no-repl
type Int32 = Prim.Types.Int32
```


## Type `Int64`
``` motoko no-repl
type Int64 = Prim.Types.Int64
```


## Type `Nat`
``` motoko no-repl
type Nat = Prim.Types.Nat
```


## Type `Nat8`
``` motoko no-repl
type Nat8 = Prim.Types.Nat8
```


## Type `Nat16`
``` motoko no-repl
type Nat16 = Prim.Types.Nat16
```


## Type `Nat32`
``` motoko no-repl
type Nat32 = Prim.Types.Nat32
```


## Type `Nat64`
``` motoko no-repl
type Nat64 = Prim.Types.Nat64
```


## Type `Principal`
``` motoko no-repl
type Principal = Prim.Types.Principal
```


## Type `Region`
``` motoko no-repl
type Region = Prim.Types.Region
```


## Type `Text`
``` motoko no-repl
type Text = Prim.Types.Text
```


## Type `Hash`
``` motoko no-repl
type Hash = Nat32
```


## Type `Iter`
``` motoko no-repl
type Iter<T> = { next : () -> ?T }
```


## Type `Order`
``` motoko no-repl
type Order = {#less; #equal; #greater}
```


## Type `Result`
``` motoko no-repl
type Result<T, E> = {#ok : T; #err : E}
```


## Type `Pattern`
``` motoko no-repl
type Pattern = {#char : Char; #text : Text; #predicate : (Char -> Bool)}
```


## Type `Time`
``` motoko no-repl
type Time = Int
```


## Type `Duration`
``` motoko no-repl
type Duration = {#days : Nat; #hours : Nat; #minutes : Nat; #seconds : Nat; #milliseconds : Nat; #nanoseconds : Nat}
```


## Type `TimerId`
``` motoko no-repl
type TimerId = Nat
```


## Type `List`
``` motoko no-repl
type List<T> = { var blocks : [var [var ?T]]; var blockIndex : Nat; var elementIndex : Nat }
```


## Module `Queue`

``` motoko no-repl
module Queue
```


### Type `Queue`
``` motoko no-repl
type Queue<T> = { var front : ?Node<T>; var back : ?Node<T>; var size : Nat }
```



### Type `Node`
``` motoko no-repl
type Node<T> = { value : T; var next : ?Node<T>; var previous : ?Node<T> }
```


## Type `Queue`
``` motoko no-repl
type Queue<T> = Queue.Queue<T>
```


## Module `Set`

``` motoko no-repl
module Set
```


### Type `Node`
``` motoko no-repl
type Node<T> = {#leaf : Leaf<T>; #internal : Internal<T>}
```



### Type `Data`
``` motoko no-repl
type Data<T> = { elements : [var ?T]; var count : Nat }
```



### Type `Internal`
``` motoko no-repl
type Internal<T> = { data : Data<T>; children : [var ?Node<T>] }
```



### Type `Leaf`
``` motoko no-repl
type Leaf<T> = { data : Data<T> }
```



### Type `Set`
``` motoko no-repl
type Set<T> = { var root : Node<T>; var size : Nat }
```


## Type `Set`
``` motoko no-repl
type Set<T> = Set.Set<T>
```


## Module `Map`

``` motoko no-repl
module Map
```


### Type `Node`
``` motoko no-repl
type Node<K, V> = {#leaf : Leaf<K, V>; #internal : Internal<K, V>}
```



### Type `Data`
``` motoko no-repl
type Data<K, V> = { kvs : [var ?(K, V)]; var count : Nat }
```



### Type `Internal`
``` motoko no-repl
type Internal<K, V> = { data : Data<K, V>; children : [var ?Node<K, V>] }
```



### Type `Leaf`
``` motoko no-repl
type Leaf<K, V> = { data : Data<K, V> }
```



### Type `Map`
``` motoko no-repl
type Map<K, V> = { var root : Node<K, V>; var size : Nat }
```


## Type `Map`
``` motoko no-repl
type Map<K, V> = Map.Map<K, V>
```


## Module `Stack`

``` motoko no-repl
module Stack
```


### Type `Stack`
``` motoko no-repl
type Stack<T> = { var top : Pure.List<T>; var size : Nat }
```


## Type `Stack`
``` motoko no-repl
type Stack<T> = Stack.Stack<T>
```


## Module `Pure`

``` motoko no-repl
module Pure
```


### Type `List`
``` motoko no-repl
type List<T> = ?(T, List<T>)
```



### Module `Map`

``` motoko no-repl
module Map
```


#### Type `Map`
``` motoko no-repl
type Map<K, V> = { size : Nat; root : Tree<K, V> }
```



#### Type `Tree`
``` motoko no-repl
type Tree<K, V> = {#red : (Tree<K, V>, K, V, Tree<K, V>); #black : (Tree<K, V>, K, V, Tree<K, V>); #leaf}
```



### Type `Map`
``` motoko no-repl
type Map<K, V> = Map.Map<K, V>
```



### Type `Queue`
``` motoko no-repl
type Queue<T> = (List<T>, Nat, List<T>)
```



### Module `Set`

``` motoko no-repl
module Set
```


#### Type `Tree`
``` motoko no-repl
type Tree<T> = {#red : (Tree<T>, T, Tree<T>); #black : (Tree<T>, T, Tree<T>); #leaf}
```



#### Type `Set`
``` motoko no-repl
type Set<T> = { size : Nat; root : Tree<T> }
```



### Type `Set`
``` motoko no-repl
type Set<T> = Set.Set<T>
```

