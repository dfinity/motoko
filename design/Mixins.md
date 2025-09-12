# Mixins

Mixins allow defining parts of an actor in their own file and then _including_ it into another actor.

## Simple example

``` motoko
mixin MyMixin() {
  var counter : Nat = 0;
  public func inc() : async () {
    counter += 1;
  };
};
```

``` motoko
import MyMixin "mo:MyMixin";

persistent actor {
  include MyMixin();
  public func get() : async Nat {
    counter
  };
}
```

When an actor includes a mixin it adds its declarations to its body. `public` definitions of the mixin also become part of the interface of the including actor. The actor has access to the `private` fields and methods of the mixin.
