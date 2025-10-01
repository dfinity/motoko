# Mixins

Mixins allow defining parts of an actor in their own file and then _including_ it into another actor.

## Mixed counter example

``` motoko
// File mixins/CounterMixin.mo
mixin(incAmount : Nat) {
  var counter : Nat = 0;
  public func inc() : async () {
    counter += incAmount;
  };
};
```

``` motoko
import CounterMixin "mo:mixins/CounterMixin";

persistent actor {
  include CounterMixin(2);
  public func incAndGet() : async Nat {
    await inc();
    counter
  };
}
```

When an actor includes a mixin it adds its declarations to its body. `public` definitions of the mixin also become part of the interface of the including actor. The actor has access to the `private` fields, types and methods of the mixin. Access to private fields is required, so that a migration expression on the main actor can migrate the composition of actor and mixin.

The fields of an actor are `stable` by default, even when included in legacy, non-persistent actors.

Mixins can be parameterized over arguments that are bound as transient lets (with renaming so they don't conflict with existing definitions). This means that if you want the mixin to have access to state of the including actor you can't just pass a variable reference, but need to wrap it up in an object. Passing already mutable objects like `core/Map`, `core/List`, `[var A]` works just fine.
