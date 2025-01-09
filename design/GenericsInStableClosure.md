# Generics in Stable Closures

Currently, type generics are not reified in Motoko runtime system but erased by the compiler. 

Therefore, when checking upgrade compatibility of stable closures, we need to make sure that it is safe to ignore the concrete types for captured generic variables.

## Safe to Ignore Concrete Types of Generics

Intuitively, the reasoning is as follows:
- Either, the generic variables in a closure are checked separately for compatibility at stable declarations which use the variables.

    ```
    class Test<X>(initial: X) {
        var content = initial;

        public func get(): X {
            content;
        };

        public func set(value: X) {
            content := value;
        };
    };


    stable let stableFunction1 = Test<Nat>(1).get; // stable () -> Nat
    stable let stableFunction2 = Test<Nat>(1).set; // stable Nat -> () 
    ```

    This cannot be changed to incompatible concrete types:
    ```
    stable let stableFunction1 = Test<Text>("...").get; // stable () -> Nat
    stable let stableFunction2 = Test<Text>("...).set; // stable Nat -> () 
    ```

- Or, if this is not the case, the generic type is isolated in the closure and cannot be operated on a new concrete type instantiation.

    ```
    class Test<X>(value: X, op: X -> ()) {
        public func run() {
            op(value);
        }
    };


    func printNat(x: Nat) {
        Prim.debugPrint(debug_show(x));
    };

    stable let stableFunction = Test<Nat>(1, printNat).run; // stable () -> ()
    ```

    If migrated to an different generic instantiation, the captured variables continue to be of the old concrete type.

    ```
    stable let stableFunction = Test<Text>("Hello", printText).run; // stable () -> ()
    run();
    ```

    `run` still accesses the old `X = Nat` declararations, incl. `printNat`. And the signature of `printNat` cannot be changed to `Text`.


## Other Rules to be Checked

The only two aspects of compatibility for generics need to be checked:
1. The generics are not swapped, i.e. captured variables retain the same type generic (e.g. according to the declaration order of the generics).

    ```
    class Test<X, Y>() {
        var first: X = ...;
        var second: Y = ...;

        public func method() {
            ... Use X and Y 
        };
    };

    stable let stableFunction = Test<Nat, Text>.method;
    ```

    Now, in the new version, I cannot e.g. swap `X` and `Y`.

    ```
    class Test<Y, X>() {
        var first: X = ...; // incompatible: Must be Y (first declared generic in scope)
        var second: Y = ...; // incompatible: Must be X (second declareed generic in scope)

        public func method() { ... };
    };
    ```

2. The type bounds of the generics are compatible.

    ```
    class Test<X>() {
        var content: X = ...;
        
        public func method() { ... };
    };

    stable let stableFunction = Test<Nat>.method;
    ```

    cannot be changed to:
    ```
    class Test<X : Text>() {
        var content: X = ...;
        
        public func method() { debugPrint(content) };
    };
    ```
