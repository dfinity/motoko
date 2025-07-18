import Prim "mo:prim";
func fail<T>() : T = Prim.trap("fail");

// Mock functions for Array module
module Array {
  public func tabulate<T>(_size : Nat, _f : Nat -> T) : [T] = [];
  public func find<T>(_array : [T], _predicate : T -> Bool) : ?T = null;
  public func findIndex<T>(_array : [T], _predicate : T -> Bool) : ?Nat = null;
  public func forEach<T>(_array : [T], _f : T -> ()) {};
  public func map<T, R>(_array : [T], _f : T -> R) : [R] = [];
  public func mapToMutable<T, R>(_array : [T], _f : T -> R) : [var R] = [var];
  public func filter<T>(_array : [T], _f : T -> Bool) : [T] = [];
  public func mapEntries<T, R>(_array : [T], _f : (T, Nat) -> R) : [R] = [];
  public func flatMap<T, R>(_array : [T], _f : T -> [R]) : [R] = [];
  public func foldRight<T, A>(_array : [T], _base : A, _f : (T, A) -> A) : A = _base;
  public func all<T>(_array : [T], _predicate : T -> Bool) : Bool = true;
  public func any<T>(_array : [T], _predicate : T -> Bool) : Bool = false;
};

// Mock functions for VarArray module
module VarArray {
  public func tabulate<T>(_size : Nat, _f : Nat -> T) : [var T] = [var];
  public func find<T>(_array : [var T], _predicate : T -> Bool) : ?T = null;
  public func findIndex<T>(_array : [var T], _predicate : T -> Bool) : ?Nat = null;
  public func forEach<T>(_array : [var T], _f : T -> ()) {};
  public func map<T, R>(_array : [var T], _f : T -> R) : [var R] = [var];
  public func mapToImmutable<T, R>(_array : [var T], _f : T -> R) : [R] = [];
  public func mapInPlace<T>(_array : [var T], _f : T -> T) {};
  public func filter<T>(_array : [var T], _f : T -> Bool) : [var T] = [var];
  public func mapEntries<T, R>(_array : [var T], _f : (T, Nat) -> R) : [var R] = [var];
  public func flatMap<T, R>(_array : [var T], _f : T -> [R]) : [var R] = [var];
  public func foldRight<T, A>(_array : [var T], _base : A, _f : (T, A) -> A) : A = _base;
  public func all<T>(_array : [var T], _predicate : T -> Bool) : Bool = true;
  public func any<T>(_array : [var T], _predicate : T -> Bool) : Bool = false;
};

// Mock functions for Iter module
type Iter<T> = { next : () -> ?T };
module Iter {
  public func forEach<T>(_iter : Iter<T>, _f : T -> ()) {};
  public func map<T, R>(_iter : Iter<T>, _f : T -> R) : Iter<R> = fail();
  public func filter<T>(_iter : Iter<T>, _f : T -> Bool) : Iter<T> = _iter;
  public func filterMap<T, R>(_iter : Iter<T>, _f : T -> ?R) : Iter<R> = fail();
  public func flatMap<T, R>(_iter : Iter<T>, _f : T -> Iter<R>) : Iter<R> = fail();
  public func takeWhile<T>(_iter : Iter<T>, _predicate : T -> Bool) : Iter<T> = _iter;
  public func dropWhile<T>(_iter : Iter<T>, _predicate : T -> Bool) : Iter<T> = _iter;
  public func zipWith<T, U, R>(_iter1 : Iter<T>, _iter2 : Iter<U>, _f : (T, U) -> R) : Iter<R> = fail();
  public func zipWith3<T, U, V, R>(_iter1 : Iter<T>, _iter2 : Iter<U>, _iter3 : Iter<V>, _f : (T, U, V) -> R) : Iter<R> = fail();
  public func all<T>(_iter : Iter<T>, _predicate : T -> Bool) : Bool = true;
  public func any<T>(_iter : Iter<T>, _predicate : T -> Bool) : Bool = false;
  public func find<T>(_iter : Iter<T>, _predicate : T -> Bool) : ?T = null;
  public func findIndex<T>(_iter : Iter<T>, _predicate : T -> Bool) : ?Nat = null;
  public func foldLeft<T, A>(_iter : Iter<T>, _base : A, _f : (A, T) -> A) : A = _base;
  public func foldRight<T, A>(_iter : Iter<T>, _base : A, _f : (T, A) -> A) : A = _base;
  public func unfold<T, S>(_init : S, _f : S -> ?(T, S)) : Iter<T> = fail();
};

// Mock functions for List module
type List<T> = {
  var blocks : [var [var ?T]];
  var blockIndex : Nat;
  var elementIndex : Nat;
};
module List {
  public func filter<T>(_list : List<T>, _predicate : T -> Bool) : List<T> = _list;
  public func filterMap<T, R>(_list : List<T>, _f : T -> ?R) : List<R> = fail();
  public func find<T>(_list : List<T>, _predicate : T -> Bool) : ?T = null;
  public func findIndex<T>(_list : List<T>, _predicate : T -> Bool) : ?Nat = null;
  public func findLastIndex<T>(_list : List<T>, _predicate : T -> Bool) : ?Nat = null;
  public func all<T>(_list : List<T>, _predicate : T -> Bool) : Bool = true;
  public func any<T>(_list : List<T>, _predicate : T -> Bool) : Bool = false;
};

// Mock functions for pure List module
module PureList {
  public func forEach<T>(_list : PureList<T>, _f : T -> ()) {};
  public func filter<T>(_list : PureList<T>, _predicate : T -> Bool) : PureList<T> = fail();
  public func partition<T>(_list : PureList<T>, _predicate : T -> Bool) : (PureList<T>, PureList<T>) = (fail(), fail());
  public func find<T>(_list : PureList<T>, _predicate : T -> Bool) : ?T = null;
  public func findIndex<T>(_list : PureList<T>, _predicate : T -> Bool) : ?Nat = null;
  public func all<T>(_list : PureList<T>, _predicate : T -> Bool) : Bool = true;
  public func any<T>(_list : PureList<T>, _predicate : T -> Bool) : Bool = false;
  public func tabulate<T>(_size : Nat, _f : Nat -> T) : PureList<T> = fail();
};

// Mock functions for Queue module
type Queue<T> = {
  var front : ?Node<T>;
  var back : ?Node<T>;
  var size : Nat;
};

type Node<T> = {
  value : T;
  var next : ?Node<T>;
  var previous : ?Node<T>;
};
module Queue {
  public func all<T>(_queue : Queue<T>, _predicate : T -> Bool) : Bool = true;
  public func any<T>(_queue : Queue<T>, _predicate : T -> Bool) : Bool = false;
  public func forEach<T>(_queue : Queue<T>, _f : T -> ()) {};
  public func map<T, R>(_queue : Queue<T>, _f : T -> R) : Queue<R> = fail();
  public func filter<T>(_queue : Queue<T>, _predicate : T -> Bool) : Queue<T> = _queue;
};

// Mock functions for pure Queue module
type PureList<T> = ?(T, PureList<T>);
type PureQueue<T> = (PureList<T>, Nat, PureList<T>);
module PureQueue {
  public func all<T>(_queue : PureQueue<T>, _predicate : T -> Bool) : Bool = true;
  public func any<T>(_queue : PureQueue<T>, _predicate : T -> Bool) : Bool = false;
  public func forEach<T>(_queue : PureQueue<T>, _f : T -> ()) {};
  public func filter<T>(_queue : PureQueue<T>, _predicate : T -> Bool) : PureQueue<T> = _queue;
  public func map<T, R>(_queue : PureQueue<T>, _f : T -> R) : PureQueue<R> = fail();
  public func filterMap<T, R>(_queue : PureQueue<T>, _f : T -> ?R) : PureQueue<R> = fail();
};

// Mock functions for Stack module
type Stack<T> = {
  var top : PureList<T>;
  var size : Nat;
};
module Stack {
  public func tabulate<T>(_size : Nat, _f : Nat -> T) : Stack<T> = fail();
  public func all<T>(_stack : Stack<T>, _predicate : T -> Bool) : Bool = true;
  public func any<T>(_stack : Stack<T>, _predicate : T -> Bool) : Bool = false;
  public func forEach<T>(_stack : Stack<T>, _f : T -> ()) {};
  public func map<T, R>(_stack : Stack<T>, _f : T -> R) : Stack<R> = fail();
  public func filter<T>(_stack : Stack<T>, _predicate : T -> Bool) : Stack<T> = _stack;
  public func filterMap<T, R>(_stack : Stack<T>, _f : T -> ?R) : Stack<R> = fail();
};

// Mock functions for Set module
type Set<T> = Set.Set<T>;
type Compare<T> = (T, T) -> { #less; #equal; #greater };
module Set {
  public type Node<T> = {
    #leaf : Leaf<T>;
    #internal : Internal<T>;
  };

  public type Data<T> = {
    elements : [var ?T];
    var count : Nat;
  };

  public type Internal<T> = {
    data : Data<T>;
    children : [var ?Node<T>];
  };

  public type Leaf<T> = {
    data : Data<T>;
  };

  public type Set<T> = {
    var root : Node<T>;
    var size : Nat;
  };
  public func retainAll<T>(_set : Set<T>, _compare : Compare<T>, _predicate : T -> Bool) : Bool = true;
  public func forEach<T>(_set : Set<T>, _f : T -> ()) {};
  public func filter<T>(_set : Set<T>, _compare : Compare<T>, _predicate : T -> Bool) : Set<T> = _set;
  public func filterMap<T, R>(_set : Set<T>, _compare : Compare<R>, _f : T -> ?R) : Set<R> = fail();
  public func all<T>(_set : Set<T>, _predicate : T -> Bool) : Bool = true;
  public func any<T>(_set : Set<T>, _predicate : T -> Bool) : Bool = false;
};

// Mock functions for pure Set module
type PureSet<T> = PureSet.Set<T>;
module PureSet {
  public type Tree<T> = {
    #red : (Tree<T>, T, Tree<T>);
    #black : (Tree<T>, T, Tree<T>);
    #leaf;
  };

  public type Set<T> = { size : Nat; root : Tree<T> };

  public func forEach<T>(_set : PureSet<T>, _f : T -> ()) {};
  public func filter<T>(_set : PureSet<T>, _compare : Compare<T>, _predicate : T -> Bool) : PureSet<T> = _set;
  public func filterMap<T, R>(_set : PureSet<T>, _compare : Compare<R>, _f : T -> ?R) : PureSet<R> = fail();
  public func all<T>(_set : PureSet<T>, _predicate : T -> Bool) : Bool = true;
  public func any<T>(_set : PureSet<T>, _predicate : T -> Bool) : Bool = false;
};

// Mock functions for Map module
type Map<K, V> = Map.Map<K, V>;
module Map {
  public type Node<K, V> = {
    #leaf : Leaf<K, V>;
    #internal : Internal<K, V>;
  };

  public type Data<K, V> = {
    kvs : [var ?(K, V)];
    var count : Nat;
  };

  public type Internal<K, V> = {
    data : Data<K, V>;
    children : [var ?Node<K, V>];
  };

  public type Leaf<K, V> = {
    // why the extra indirection?
    data : Data<K, V>;
  };

  public type Map<K, V> = {
    var root : Node<K, V>;
    var size : Nat;
  };
  public func forEach<K, V>(_map : Map<K, V>, _f : (K, V) -> ()) {};
  public func filter<K, V>(_map : Map<K, V>, _compare : Compare<K>, _predicate : (K, V) -> Bool) : Map<K, V> = _map;
  public func all<K, V>(_map : Map<K, V>, _predicate : (K, V) -> Bool) : Bool = true;
  public func any<K, V>(_map : Map<K, V>, _predicate : (K, V) -> Bool) : Bool = false;
  public func toText<K, V>(_map : Map<K, V>, _keyToText : K -> Text, _valueToText : V -> Text) : Text = "";
};

// Mock functions for pure Map module
type PureMap<K, V> = PureMap.Map<K, V>;
module PureMap {
  public type Map<K, V> = {
    size : Nat;
    root : Tree<K, V>;
  };
  public type Tree<K, V> = {
    #red : (Tree<K, V>, K, V, Tree<K, V>);
    #black : (Tree<K, V>, K, V, Tree<K, V>);
    #leaf;
  };
  public func all<K, V>(_map : PureMap<K, V>, _predicate : (K, V) -> Bool) : Bool = true;
  public func any<K, V>(_map : PureMap<K, V>, _predicate : (K, V) -> Bool) : Bool = false;
  public func forEach<K, V>(_map : PureMap<K, V>, _f : (K, V) -> ()) {};
  public func filter<K, V>(_map : PureMap<K, V>, _compare : Compare<K>, _predicate : (K, V) -> Bool) : PureMap<K, V> = _map;
  public func toText<K, V>(_map : PureMap<K, V>, _keyToText : K -> Text, _valueToText : V -> Text) : Text = "";
};

// Mock functions for Result module
type Result<T, E> = { #ok : T; #err : E };
module Result {
  public func mapOk<T, R, E>(_result : Result<T, E>, _f : T -> R) : Result<R, E> = #err(fail());
  public func mapErr<T, E, F>(_result : Result<T, E>, _f : E -> F) : Result<T, F> = #err(fail());
  public func forOk<T, E>(_result : Result<T, E>, _f : T -> ()) {};
  public func forErr<T, E>(_result : Result<T, E>, _f : E -> ()) {};
};

// Mock functions for Option module
module Option {
  public func map<T, R>(_option : ?T, _f : T -> R) : ?R = null;
};

// Mock functions for Tuple modules
module Tuple2 {
  public func makeToText<A, B>(_f1 : A -> Text, _f2 : B -> Text) : (A, B) -> Text = func(_a, _b) = "";
};

module Tuple3 {
  public func makeToText<A, B, C>(_f1 : A -> Text, _f2 : B -> Text, _f3 : C -> Text) : (A, B, C) -> Text = func(_a, _b, _c) = "";
};

// Mock comparison and conversion functions
func natCompare(_a : Nat, _b : Nat) : { #less; #equal; #greater } = #equal;
func textCompare(_a : Text, _b : Text) : { #less; #equal; #greater } = #equal;
func natToText(_n : Nat) : Text = "";

let ar = [1, 2, 3];
let varAr : [var Nat] = [var 1, 2, 3];
let iter : Iter<Nat> = { next = func() : ?Nat = null };
let iterText : Iter<Text> = { next = func() : ?Text = null };
let iterChar : Iter<Char> = { next = func() : ?Char = null };
let list : List<Nat> = fail();
let pureList : PureList<Nat> = null;
let queue : Queue<Nat> = fail();
let pureQueue : PureQueue<Nat> = fail();
let stack : Stack<Nat> = fail();
let set : Set<Nat> = fail();
let pureSet : PureSet<Nat> = fail();
let mapInstance : Map<Nat, Text> = fail();
let pureMap : PureMap<Nat, Text> = fail();
let resultOk : Result<Nat, Text> = #ok(1);
let resultErr : Result<Nat, Text> = #err("");
let optionSome : ?Nat = ?1;

// Array module explicit type instantiation tests
let _ = Array.tabulate(4, func i = i * 2);
let _ = Array.find(ar, func x = x > 8);
let _ = Array.findIndex(['A', 'B', 'C'], func x = x == 'C');
let _ = Array.forEach(ar, func _ {});
let _ = Array.map(ar, func x = x * 2);
let _ = Array.mapToMutable<Nat, Int>(ar, func x = x * 2);
let _ = Array.filter(ar, func x = x % 2 == 0);
let _ = Array.mapEntries(ar, func(x, i) = i * x);
let _ = Array.flatMap(ar, func x = [x, -x]);
let _ = Array.foldRight(ar, "", func(x, acc) = natToText(x) # acc);
let _ = Array.all(ar, func x = x > 0);
let _ = Array.any(ar, func x = x > 3);

// VarArray module explicit type instantiation tests
let _ = VarArray.tabulate<Nat>(4, func i = i * 2);
let _ = VarArray.find(varAr, func x = x > 8);
let _ = VarArray.findIndex([var 'A', 'B', 'C'], func x = x == 'C');
let _ = VarArray.forEach(varAr, func(x) {});
let _ = VarArray.map<Nat, Int>(varAr, func x = x * 2);
let _ = VarArray.mapToImmutable(varAr, func x = x * 2);
let _ = VarArray.mapInPlace(varAr, func x = x * 3);
let _ = VarArray.filter(varAr, func x = x % 2 == 0);
let _ = VarArray.mapEntries<Nat, Nat>(varAr, func(x, i) = i * x);
let _ = VarArray.flatMap<Nat, Int>(varAr, func x = [x, -x]);
let _ = VarArray.foldRight(varAr, "", func(x, acc) = natToText(x) # acc);
let _ = VarArray.all(varAr, func x = x > 0);
let _ = VarArray.any(varAr, func x = x > 3);

// Iter module explicit type instantiation tests
let _ = Iter.forEach(iter, func _ {});
let _ = Iter.map(iter, func x = x * 2);
let _ = Iter.filter(iter, func x = x % 2 == 0);
let _ = Iter.filterMap(iter, func x = if (x % 2 == 0) ?x else null);
let _ = Iter.flatMap(iter, func x = iter);
let _ = Iter.takeWhile(iter, func x = x < 4);
let _ = Iter.dropWhile(iter, func x = x < 4);
let _ = Iter.zipWith(iterText, iterText, func(a, b) = a # b);
let _ = Iter.zipWith3(iterText, iterText, iterText, func(a, b, c) = a # b # c);
let _ = Iter.all(iter, func x = x < 4);
let _ = Iter.any(iter, func x = x == 2);
let _ = Iter.find(iter, func x = x % 2 == 0);
let _ = Iter.findIndex(iterChar, func x = x == 'C');
let _ = Iter.foldLeft(iterText, "S", func(acc, x) = "(" # acc # x # ")");
let _ = Iter.foldRight(iterText, "S", func(x, acc) = "(" # x # acc # ")");
let _ = Iter.unfold(1, func x = if (x <= 3) ?(x, x + 1) else null);

// List module explicit type instantiation tests
let _ = List.filter(list, func x = x % 2 == 0);
let _ = List.filterMap<Nat, Nat>(list, func x = if (x % 2 == 0) ?(x * 2) else null);
let _ = List.find(list, func x = x > 8);
let _ = List.findIndex(list, func i = i % 2 == 0);
let _ = List.findLastIndex(list, func i = i % 2 == 0);
let _ = List.all(list, func x = x > 1);

// pure List module explicit type instantiation tests
let _ = PureList.forEach(pureList, func n = ());
let _ = PureList.filter(pureList, func n = n != 1);
let _ = PureList.partition(pureList, func n = n != 1);
let _ = PureList.find(pureList, func n = n > 1);
let _ = PureList.findIndex(pureList, func n = n > 1);
let _ = PureList.all(pureList, func n = n > 1);
let _ = PureList.any(pureList, func n = n > 1);
let _ = PureList.tabulate(3, func n = n * 2);

// Queue module explicit type instantiation tests
let _ = Queue.all(queue, func x = x % 2 == 0);
let _ = Queue.any(queue, func x = x > 2);
let _ = Queue.forEach(queue, func _ {});
let _ = Queue.map<Nat, Nat>(queue, func x = x * 2);
let _ = Queue.filter(queue, func x = x % 2 == 0);

// pure Queue module explicit type instantiation tests
let _ = PureQueue.all(pureQueue, func n = n > 1);
let _ = PureQueue.any(pureQueue, func n = n > 1);
let _ = PureQueue.forEach(pureQueue, func _ {});
let _ = PureQueue.filter(pureQueue, func n = n != 1);
let _ = PureQueue.map(pureQueue, func n = n * 2);
let _ = PureQueue.filterMap(pureQueue, func n = if (n % 2 == 0) ?n else null);

// Stack module explicit type instantiation tests
let _ = Stack.tabulate<Nat>(3, func i = 2 * i);
let _ = Stack.all(stack, func n = n % 2 == 0);
let _ = Stack.any(stack, func n = n == 2);
let _ = Stack.forEach(stack, func _ {});
let _ = Stack.map<Nat, Nat>(stack, func n = 2 * n);
let _ = Stack.filter(stack, func n = n % 2 == 0);
let _ = Stack.filterMap<Nat, Nat>(stack, func n = if (n % 2 == 0) ?n else null);

// Set module explicit type instantiation tests
let _ = Set.retainAll(set, natCompare, func n = n % 2 == 0);
let _ = Set.forEach(set, func _ {});
let _ = Set.filter(set, natCompare, func n = n % 2 == 0);
let _ = Set.filterMap<Nat, Text>(
  set,
  textCompare,
  func n = if (n % 2 == 0) ?natToText(n) else null,
);
let _ = Set.all(set, func n = n < 10);
let _ = Set.any(set, func n = n > 5);

// pure Set module explicit type instantiation tests
let _ = PureSet.forEach(pureSet, func _ {});
let _ = PureSet.filter(pureSet, natCompare, func n = n % 2 == 0);
let _ = PureSet.filterMap<Nat, Text>(
  pureSet,
  textCompare,
  func n = if (n % 2 == 0) ?natToText(n) else null,
);
let _ = PureSet.all(pureSet, func n = n < 10);
let _ = PureSet.any(pureSet, func n = n > 5);

// Map module explicit type instantiation tests
let _ = Map.forEach(mapInstance, func(key, value) {});
let _ = Map.filter(mapInstance, natCompare, func(key, value) = key % 2 == 0);
let _ = Map.all(mapInstance, func(k, v) = v == natToText(k));
let _ = Map.any(mapInstance, func(k, v) = k >= 0);
let _ = Map.toText(mapInstance, natToText, func t = t);

// pure Map module explicit type instantiation tests
let _ = PureMap.all(pureMap, func(k, v) = v == natToText(k));
let _ = PureMap.any(pureMap, func(k, v) = k >= 0);
let _ = PureMap.forEach(pureMap, func(key, value) {});
let _ = PureMap.filter(
  pureMap,
  natCompare,
  func(key, value) {
    key % 2 == 0;
  },
);
let _ = PureMap.toText(pureMap, natToText, func t = t);

// Result module explicit type instantiation tests
let _ = Result.mapOk(resultOk, func x = x * 2);
let _ = Result.mapErr(resultErr, func x = x # "!");
let _ = Result.forOk(resultOk, func x {});
let _ = Result.forErr(resultErr, func x {});

// Option module explicit type instantiation tests
let _ = Option.map(optionSome, func x = x + 1);

// Tuple module explicit type instantiation tests
// TODO: Can we make functions like func x = x work?
// let _ = Tuple2.makeToText(natToText, func x = x);
// let _ = Tuple3.makeToText(natToText, func x = x, natToText);

//SKIP comp
//SKIP run
//SKIP run-drun
//SKIP run-ir
//SKIP run-low
