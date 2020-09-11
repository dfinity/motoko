Tranferring Funds with calls
----------------------------

This is strawman proposal for extending Motoko with support for funds
(a.k.a payments). Funds is the terminology in the public spec so we'll
stick to that.

The basic idea in the System API is to represent a balance or
amount of funds as a (sparse) vector of `u64`, indexed by
*unit*. Two well-known (i.e. built-in) units are `icpt` and `cycle`,
but others are envisioned.

Every canister maintains a current balance of funds.

Every message send is accompanied by (possibly all-zero) additional
funds, drawn from the funds available to the caller.  On successful
reply, the received funds can be accepted, in part or in whole, and
added to the canister balance, by specifying a subset of the allocated
funds to consume.  The remainder of the received funds is refunded to
the sender (via the success callback). Thus every success callback
receives a refund (the one sent by the callee's reply). Though
automatically added to the senders' current balance, the refund is
available for inspection and cannot, I believe, be deduced from the
difference of the old and new balance prior to the call (due to
possible interference).


## Motoko design(s)

This System API design suggest the following extension of Motoko to deal with funds.

We first define a type for funds:

```
type Funds = {
  icpts: Nat64;
  cycles : Nat64;
  // custom : [ (unit: Nat64, amount: Nat64) ];
};
```

The record type is natural in negative positions where you can omit fields, but awkward in positive ones, where you can't.
Maybe it would be better to just use a sparse vector (c.f. the `custom` array) or more abstract finite map (a functional object?).

Whatever representation we choose for `Funds`,
*shared* functions need selective access to any funds received, which we do by extending the `Context` type - used
to type `<shared-pat>` patterns and provide caller information - with a `funds` field:

```
type Context = {
  caller : Principal; // as before
  funds : Funds;
}
```

On the platform, only the `msg_reply` call can consume transferred funds.
A system `msg_reply` corresponds to a Motoko `return` from a shared function (or async block).
So we extend (async) `return` and shared function call with optional `funds` arguments.

```
<exp> := (accept <exp>)? return <exp>          // extended return
      | <exp> <typ-args>? <exp> <with <exp>>?  // extended call
```

With these two ingredients we can write a skeletal crowd-funding app:

```
actor CrowdFund {
 ...
 shared {funds} func invest() : Nat{
   if (funds.icpt == 0) throw Error.error("cheapSkate"); // fail, refund all
   let balance = Balance(/*[]*/).icpts;
   if (func.icpts > individualLimit) throw Error.error("dogdy funds"); // fail, accept nothing (refund all)
   if (balance + funds.icpts > ceiling)
     return ("Ceiling reached"); // ordinary return, accept nothing (refund all)
   let required =  {icpts = Min(Ceiling - balance, funds.icpts), cycles = 0; /* custom = [] */ };
   accept required return "Thanks!"; // accepting return: accept required & refund rest
 };

}

// Client code

let funds = {icpt = 10; cycles = 0; /* custom = [] */}; // this get
... CrowdFund.invest() with funds ...
```

Here, we assume access to a `Balance` function (primitive) to give us access to the canister's current funds (which can change at awaits and sends - `Balance.cycles()` presumably changes
at each call).

(For custom units, the `Balance` function would need to take an array of `unit`s to indicate the units of interest as there is (currently) no way
to enumerate the available, non-zero units.)

## Refunding Awaits

The platform will return refunds to success callbacks.
Refunds are automatically added to the caller's balance, but it may still be useful to discover the actual amounts refunded.

For Motoko, this means receiving refunds on successfull awaits.

We have several options here:

1. Extend (existing) `await` so it always returns a pair of funds and value.
```
 let (refund (* : Fund *),msg) = await CrowdFound.invest() with funds;
 ...
```
  - breaking change
  - awkward to use for common code that doesn't care about refunds - the value we typically care about is always the second component.

2. Introduce a variant of `await`, say `receive`, that returns a pair of refund and value (burn a keyword).
```
 let msg = await CrowdFound.invest() with funds; // refund ignored
 let (refund (* : Fund *),msg) = receive CrowdFound.invest() with funds; // refund considered
```

3. Extend the `Async<T>` type with a `refund` member that extracts the refund.
```
	let a = CrowdFund.invest() with funds;
	let res = await a;
    let () = a.refund();
```
  + no impact on ordinary await
  - awkward to access refund: requires one to name the async value to access the refund
  - refund can be accessed before completion - what should we do: return the *null* refund, trap or return an awkward option?

4. extend the async `Async<T>` type itself with a member that returns the "same" async, but with the refund exposed.
```
   type Async<T> = {
     withRefund : () -> Async<(Fund,T)>
   }
...

let (refund, res) = await (CrowdFound.invest() with funds).withRefund();
...
```
  + no new syntax
  + selective interest in refund
  - uses polymorphic recursion. Not sure we can get away with this in our implementation.

5. Extend typing/evaluation of shared function calls so an extended `with` call returns an async pair (or record), existing call/await semantics unchanged.
```
 let msg = await CrowdFound.invest(); // ordinary call gets result (no record of refund)

 let (refund (* : Fund *),msg) = await (CrowdFound.invest() with funds); // `with` call gets pair of refund and result.
```
  - the function return type doesn't match the adulterated type of an extended application. Yuck.


## Puzzles

Suppose we wanted to purchase units from a supplier canister? How would one achieve that? It looks like we can only send units and receive a refund of at most that amount.
Do we need send a user-level callback on which the supplier can send the purchased units. Can we guarantee that they deliver the amount requested?


