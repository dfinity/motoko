# Generics in Persistent Closures

Currently, type generics are not reified in Motoko runtime system but erased by the compiler. 

Therefore, when checking upgrade compatibility of persistent closures, we need to make sure that it is safe to ignore the concrete types for captured generic variables.

## Safe to Ignore Concrete Types of Generics

Intuitively, the reasoning is as follows:
- Either, the generic variables in a closure are checked separately for compatibility at persistent declarations which use the variables.

    ```
    persistent actor {
        persistent class Test<X>(initial: X) {
            var content = initial;

            public func get(): X {
                content;
            };

            public func set(value: X) {
                content := value;
            };
        };


        let persistentFunction1 = Test<Nat>(1).get; // persistent () -> Nat
        let persistentFunction2 = Test<Nat>(1).set; // persistent Nat -> ()
    }
    ```

    This cannot be changed to incompatible concrete types:
    ```
    persistent actor {
        // ...
        let persistentFunction1 = Test<Text>("...").get; // persistent () -> Nat
        let persistentFunction2 = Test<Text>("...).set; // persistent Nat -> () 
    }
    ```

- Or, if this is not the case, the generic type is isolated in the closure and cannot be operated on a new concrete type instantiation.

    ```
    persistent actor {
        persistent class Test<X>(value: X, op: X -> ()) {
            public func run() {
                op(value);
            }
        };


        persistent func printNat(x: Nat) {
            Prim.debugPrint(debug_show(x));
        };

        let persistentFunction = Test<Nat>(1, printNat).run; // persistent () -> ()
    }
    ```

    If migrated to an different generic instantiation, the captured variables continue to be of the old concrete type.

    ```
    persistent actor {
        // ...
        let persistentFunction = Test<Text>("Hello", printText).run; // persistent () -> ()
        run();
    }
    ```

    `run` still accesses the old `X = Nat` declararations, incl. `printNat`. And the signature of `printNat` cannot be changed to `Text`.


## Other Rules to be Checked

The only two aspects of compatibility for generics need to be checked:
1. The generics are not swapped, i.e. captured variables retain the same type generic (e.g. according to the declaration order of the generics).

    ```
    persistent actor {
        persistent class Test<X, Y>() {
            var first: X = ...;
            var second: Y = ...;

            public func method() {
                ... Use X and Y 
            };
        };

        let persistentFunction = Test<Nat, Text>.method;
    }
    ```

    Now, in the new version, I cannot e.g. swap `X` and `Y`.

    ```
    persistent class Test<Y, X>() {
        var first: X = ...; // incompatible: Must be Y (first declared generic in scope)
        var second: Y = ...; // incompatible: Must be X (second declareed generic in scope)

        public func method() { ... };
    };
    ```

2. The type bounds of the generics are compatible.

    ```
    persistent actor {
        persistent class Test<X>() {
            var content: X = ...;
            
            public func method() { ... };
        };

        let persistentFunction = Test<Nat>.method;
    }
    ```

    cannot be changed to:
    ```
    persistent actor {
        persistent class Test<X : Text>() {
            var content: X = ...;
            
            public func method() { debugPrint(content) };
        };
        // ...
    }
    ```
