# Message inspection

On the Internet Computer, a canister can selectively *inspect* and *accept* or *decline* ingress messages submitted through the HTTP interface:

> A canister can inspect ingress messages before executing them. When the IC receives an update call from a user, the IC will use the canister method `canister_inspect_message` to determine whether the message shall be accepted. If the canister is empty (i.e. does not have a Wasm module), then the ingress message will be rejected. If the canister is not empty and does not implement `canister_inspect_message`, then the ingress message will be accepted.
>
> In `canister_inspect_message`, the canister can accept the message by invoking `ic0.accept_message : () → ()`. This function traps if invoked twice. If the canister traps in `canister_inspect_message` or does not call `ic0.accept_message`, then the access is denied.
>
> The `canister_inspect_message` is *not* invoked for HTTP query calls, inter-canister calls or calls to the management canister.
>
> —  [IC Interface Specification](https://smartcontracts.org/docs/current/references/ic-interface-spec/#ingress-message-inspection)

Message inspection mitigates some denial of service attacks, designed to drain canisters of cycles by placing unsolicited free calls.

REMARK: You can think of method inspection as providing the "Collect call from *name*. Do you accept charges?" prologue of an old-fashioned, operator-assisted, collect phone call.

In Motoko, actors can elect to inspect and accept or decline ingress messages by declaring a particular `system` function called `inspect`. Given a record of message attributes, this function produces a `Bool` that indicates whether to accept or decline the message by returning `true` or `false`. The function is invoked (by the system) on each ingress message. Similar to a query, any side-effects of an invocation are discarded and transient. A call that traps due to some fault has the same result as returning `false` (message declination).

Unlike other system functions, that have a fixed argument type, the argument type of `inspect` depends on the interface of the enclosing actor. In particular, the formal argument of `inspect` is a record of fields of the following types:

-   `caller : Principal`: the principal, possibly anonymous, of the caller of the message;

-   `arg : Blob`: the raw, binary content of the message argument;

-   `msg : <variant>`: a variant of *decoding* functions, where `<variant> == {…​; #<id>: () → T; …​}` contains one variant per shared function, `<id>`, of the actor. The variant’s tag identifies the function to be called; The variant’s argument is a function that, when applied, returns the (decoded) argument of the call as a value of type `T`.

Using a variant, tagged with `#<id>`, allows the return type, `T`, of the decoding function to vary with the argument type (also `T`) of the shared function `<id>`.

The variant’s argument is a function so that one can avoid the expense of message decoding (when appropriate).

Exploiting subtyping, the formal argument can omit record fields it does not require, or selectively ignore the arguments of particular shared functions, for example, in order to simply dispatch on the name of a function without inspecting its actual argument.

<div class="note">

Confusingly, a `shared query` function *can* be called using a regular HTTP call to obtain a certified response: this is why the variant type also includes `shared query` functions.

</div>

<div class="warning">

An actor that fails to declare system field `inspect` will simply accept all ingress messages.

</div>

<div class="warning">

System function `inspect` should **not** be used for definitive access control. This is because `inspect` is executed by a single replica, without full consensus, and its result could be spoofed by a malicious boundary node. Reliable access control checks can only be performed within the `shared` functions guarded by `inspect`.

</div>

## Example

A simple, contrived example of method inspection is a counter actor, that inspects some of its messages in detail, and others only superficially:

``` motoko
import Principal = "mo:base/Principal";

actor {

   var c = 0;

   public func inc() : async () { c += 1 };
   public func set(n : Nat) : async () { c := n };
   public query func read() : async Nat { c };
   public func reset() : () { c := 0 }; // oneway

   system func inspect(
     {
       caller : Principal;
       arg : Blob;
       msg : {
         #inc : () -> ();
         #set : () -> Nat;
         #read : () -> ();
         #reset : () -> ();
       }
     }) : Bool {
    if (Principal.isAnonymous(caller)) return false;
    if (arg.size() > 512) return false;
    switch (msg) {
      case (#inc _) { true };
      case (#set n) { n() != 13 };
      case (#read _) { true };
      case (#reset _) { false };
    }
  }
};
```

Note that, due to subtyping, all of the following variations, in order of increasing argument specificity, are legal definitions of `inspect`.

Blanket denial of all ingress messages, ignoring further information:

``` motoko no-repl
   system func inspect({}) : Bool { false }
```

Declining anonymous calls:

``` motoko no-repl
   system func inspect({ caller : Principal }) : Bool {
     not (Principal.isAnonymous(caller));
   }
```

Declining large messages, based on \`arg’s size (in bytes).

``` motoko no-repl
   system func inspect({ arg : Blob }) : Bool {
     arg.size() <= 512;
   }
```

Declining messages by name only, ignoring message arguments (note the use of type `Any` as message argument variants):

``` motoko no-repl
   system func inspect(
     {
       msg : {
         #inc : Any;
         #set : Any;
         #read : Any;
         #reset : Any;
       }
     }) : Bool {
    switch (msg) {
      case ((#set _) or (#reset _)) { false };
      case _ { true }; // allow inc and read
    }
  }
```

A combination of the previous three, specifying the argument types of some variants while ignoring others at type `Any` and using pattern matching to conflate identical cases.

``` motoko no-repl
   system func inspect(
     {
       caller : Principal;
       arg : Blob;
       msg : {
         #inc : Any;
         #set : () -> Nat;
         #read : Any;
         #reset : Any;
       }
     }) : Bool {
    if (Principal.isAnonymous(caller)) return false;
    if (arg.size() > 512) return false;
    switch (msg) {
      case (#set n) { n() != 13 };
      case (#reset _) { false };
      case _ { true }; // allow inc and read
    }
  }
```

## Tips on authoring `inspect`

Implementing `inspect` after the fact, once all shared functions of an actor have already been implemented, can be tedious, since you’ll need to declare a correctly typed variant for each shared function. A simple trick is to first implement the function *incorrectly*, with a `()` argument, compile the code, then use the compiler’s error message to obtain the required argument type.

For example, in the actor from the previous section, incorrectly declaring:

``` motoko no-repl
   system func inspect() : Bool {
     false
   }
```

forces the compiler to report the expected type below:

``` motoko no-repl
Inspect.mo:13.4-15.5: type error [M0127], system function inspect is declared with type
  () -> Bool
instead of expected type
  {
    arg : Blob;
    caller : Principal;
    msg :
      {
        #inc : () -> ();
        #read : () -> ();
        #reset : () -> ();
        #set : () -> Nat
      }
  } -> Bool
```

which you can now cut-and-paste into your code.
