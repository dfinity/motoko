/// Common types used throughout the core package.
///
/// Example usage:
///
/// ```motoko name=import
/// import { type Result; type Iter } "mo:core/Types";
///
/// // Result for error handling
/// let result : Result<Int, Text> = #ok(42);
///
/// // Iterator for sequences
/// let iter : Iter<Nat> = { next = func() { ?1 } };
/// ```

import Prim "mo:⛔";

module {
  public type Blob = Prim.Types.Blob;
  public type Bool = Prim.Types.Bool;
  public type Char = Prim.Types.Char;
  public type Error = Prim.Types.Error;
  public type ErrorCode = Prim.ErrorCode;
  public type Float = Prim.Types.Float;
  public type Int = Prim.Types.Int;
  public type Int8 = Prim.Types.Int8;
  public type Int16 = Prim.Types.Int16;
  public type Int32 = Prim.Types.Int32;
  public type Int64 = Prim.Types.Int64;
  public type Nat = Prim.Types.Nat;
  public type Nat8 = Prim.Types.Nat8;
  public type Nat16 = Prim.Types.Nat16;
  public type Nat32 = Prim.Types.Nat32;
  public type Nat64 = Prim.Types.Nat64;
  public type Principal = Prim.Types.Principal;
  public type Region = Prim.Types.Region;
  public type Text = Prim.Types.Text;

  public type Hash = Nat32;
  public type Iter<T> = { next : () -> ?T };
  public type Order = { #less; #equal; #greater };
  public type Result<T, E> = { #ok : T; #err : E };
  public type Pattern = {
    #char : Char;
    #text : Text;
    #predicate : (Char -> Bool)
  };
  public type Time = Int;
  public type Duration = {
    #days : Nat;
    #hours : Nat;
    #minutes : Nat;
    #seconds : Nat;
    #milliseconds : Nat;
    #nanoseconds : Nat
  };
  public type TimerId = Nat;

  public type List<T> = {
    var blocks : [var [var ?T]];
    var blockIndex : Nat;
    var elementIndex : Nat
  };

  public module Queue {
    public type Queue<T> = {
      var front : ?Node<T>;
      var back : ?Node<T>;
      var size : Nat
    };

    public type Node<T> = {
      value : T;
      var next : ?Node<T>;
      var previous : ?Node<T>
    }
  };
  public type Queue<T> = Queue.Queue<T>;

  public module Set {
    public type Node<T> = {
      #leaf : Leaf<T>;
      #internal : Internal<T>
    };

    public type Data<T> = {
      elements : [var ?T];
      var count : Nat
    };

    public type Internal<T> = {
      data : Data<T>;
      children : [var ?Node<T>]
    };

    public type Leaf<T> = {
      data : Data<T>
    };

    public type Set<T> = {
      var root : Node<T>;
      var size : Nat
    }
  };
  public type Set<T> = Set.Set<T>;

  public module Map {
    public type Node<K, V> = {
      #leaf : Leaf<K, V>;
      #internal : Internal<K, V>
    };

    public type Data<K, V> = {
      kvs : [var ?(K, V)];
      var count : Nat
    };

    public type Internal<K, V> = {
      data : Data<K, V>;
      children : [var ?Node<K, V>]
    };

    public type Leaf<K, V> = {
      data : Data<K, V>
    };

    public type Map<K, V> = {
      var root : Node<K, V>;
      var size : Nat
    }
  };

  public type Map<K, V> = Map.Map<K, V>;

  public module Stack {
    public type Stack<T> = {
      var top : Pure.List<T>;
      var size : Nat
    }
  };
  public type Stack<T> = Stack.Stack<T>;

  public module Pure {
    public type List<T> = ?(T, List<T>);

    public module Map {
      public type Map<K, V> = {
        size : Nat;
        root : Tree<K, V>
      };
      public type Tree<K, V> = {
        #red : (Tree<K, V>, K, V, Tree<K, V>);
        #black : (Tree<K, V>, K, V, Tree<K, V>);
        #leaf
      };

    };
    public type Map<K, V> = Map.Map<K, V>;

    public type Queue<T> = (List<T>, Nat, List<T>);

    public module Set {
      public type Tree<T> = {
        #red : (Tree<T>, T, Tree<T>);
        #black : (Tree<T>, T, Tree<T>);
        #leaf
      };

      public type Set<T> = { size : Nat; root : Tree<T> }
    };

    public type Set<T> = Set.Set<T>;

  }
}
