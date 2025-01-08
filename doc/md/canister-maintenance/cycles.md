---
sidebar_position: 1
---


# Cycles



Usage of a canister's resources on ICP is measured and paid for in [cycles](/docs/current/developer-docs/defi/cycles/converting_icp_tokens_into_cycles).

In Motoko programs deployed on ICP, each actor represents a canister and has an associated balance of cycles. The ownership of cycles can be transferred between actors. Cycles are selectively sent and received through shared function calls. A caller can choose to transfer cycles with a call, and a callee can choose to accept cycles that are made available by the caller. Unless explicitly instructed, no cycles are transferred by callers or accepted by callees.

Callees can accept all, some, or none of the available cycles up to limit determined by their actor’s current balance. Any remaining cycles are refunded to the caller. If a call traps, all its accompanying cycles are automatically refunded to the caller without loss.

In future, we may see Motoko adopt dedicated syntax and types to support safer programming with cycles. For now, we provide a temporary way to manage cycles through a low-level imperative API provided by the [ExperimentalCycles](../base/ExperimentalCycles.md) library in package `base`.

:::note

This library is subject to change and likely to be replaced by more high-level support for cycles in later versions of Motoko.

:::

## The [`ExperimentalCycles`](../base/ExperimentalCycles.md) Library

The [`ExperimentalCycles`](../base/ExperimentalCycles.md) library provides imperative operations for observing an actor’s current balance of cycles, transferring cycles and observing refunds.

The library provides the following operations:

- `func balance() : (amount : Nat)`: Returns the actor’s current balance of cycles as `amount`. Function `balance()` is stateful and may return different values after calls to `accept(n)`, calling a function after `add`ing cycles, or resuming from `await` which reflects a refund.

- `func available() : (amount : Nat)`: Returns the currently available `amount` of cycles. This is the amount received from the current caller, minus the cumulative amount `accept`ed so far by this call. On exit from the current shared function or `async` expression via `return` or `throw` any remaining available amount is automatically refunded to the caller.

- `func accept<system>(amount : Nat) : (accepted : Nat)`: Transfers `amount` from `available()` to `balance()`. It returns the amount actually transferred, which may be less than requested, for example, if less is available, or if canister balance limits are reached. Requires `system` capability.

- `func add<system>(amount : Nat) : ()`: Indicates the additional amount of cycles to be transferred in the next remote call, i.e. evaluation of a shared function call or `async` expression. Upon the call, but not before, the total amount of units `add`ed since the last call is deducted from `balance()`. If this total exceeds `balance()`, the caller traps, aborting the call. Requires `system` capability.

- `func refunded() : (amount : Nat)`: Reports the `amount` of cycles refunded in the last `await` of the current context, or zero if no await has occurred yet. Calling `refunded()` is solely informational and does not affect `balance()`. Instead, refunds are automatically added to the current balance, whether or not `refunded` is used to observe them.

:::danger

Since cycles measure computational resources spent, the value of `balance()` generally decreases from one shared function call to the next.

The implicit register of added amounts, incremented on each `add`, is reset to zero on entry to a shared function, and after each shared function call or on resume from an await.

:::

### Example

To illustrate, we will now use the [`ExperimentalCycles`](../base/ExperimentalCycles.md) library to implement a simple piggy bank program for saving cycles.

Our piggy bank has an implicit owner, a `benefit` callback and a fixed `capacity`, all supplied at time of construction. The callback is used to transfer withdrawn amounts.

``` motoko name=PiggyBank file=../examples/PiggyBank.mo
```

The owner of the bank is identified with the implicit caller of constructor `PiggyBank()`, using the shared pattern, `shared(msg)`. Field `msg.caller` is a [`Principal`](../base/Principal.md) and is stored in private variable `owner` for future reference. See [principals and caller identification](../writing-motoko/caller-id.md) for more explanation of this syntax.

The piggy bank is initially empty, with zero current `savings`.

Only calls from `owner` may:

-   Query the current `savings` of the piggy bank (function `getSavings()`), or

-   Withdraw amounts from the savings (function `withdraw(amount)`).

The restriction on the caller is enforced by the statements `assert (msg.caller == owner)`, whose failure causes the enclosing function to trap without revealing the balance or moving any cycles.

Any caller may `deposit` an amount of cycles, provided the savings will not exceed `capacity`, breaking the piggy bank. Because the deposit function only accepts a portion of the available amount, a caller whose deposit exceeds the limit will receive an implicit refund of any unaccepted cycles. Refunds are automatic and ensured by the ICP infrastructure.

Since the transfer of cycles is unidirectional from caller to callee, retrieving cycles requires the use of an explicit callback using the `benefit` function, taken by the constructor as an argument. Here, `benefit` is called by the `withdraw` function, but only after authenticating the caller as `owner`. Invoking `benefit` in `withdraw` inverts the caller/caller relationship, allowing cycles to flow upstream.

Note that the owner of the `PiggyBank` could supply a callback that rewards a beneficiary distinct from `owner`.

Here’s how an owner, `Alice`, might use an instance of `PiggyBank`:

``` motoko include=PiggyBank file=../examples/Alice.mo
```

`Alice` imports the `PiggyBank` actor class as a library so she can create a new `PiggyBank` actor on demand.

Most of the action occurs in `Alice`'s `test()` function:

- Alice dedicates `10_000_000_000_000` of her own cycles for running the piggy bank by calling `Cycles.add(10_000_000_000_000)` just before creating a new instance, `porky`, of the `PiggyBank`, passing callback `Alice.credit` and capacity (`1_000_000_000`). Passing `Alice.credit` nominates `Alice` as the beneficiary of withdrawals. The `10_000_000_000_000` cycles, minus a small installation fee, are credited to `porky`'s balance without any further action by the program's initialization code. You can think of this as an electric piggy bank that consumes its own resources as its used. Since constructing a `PiggyBank` is asynchronous, `Alice` needs to `await` the result.

- After creating `porky`, she first verifies that the `porky.getSavings()` is zero using an `assert`.

- `Alice` dedicates `1_000_000` of her cycles (`Cycles.add<system>(1_000_000)`) to transfer to `porky` with the next call to `porky.deposit()`. The cycles are only consumed from Alice’s balance if the call to `porky.deposit()` succeeds.

- `Alice` now withdraws half the amount, `500_000`, and verifies that `porky`'s savings have halved. `Alice` eventually receives the cycles via a callback to `Alice.credit()`, initiated in `porky.withdraw()`. Note the received cycles are precisely the cycles `add`ed in `porky.withdraw()`, before it invokes its `benefit` callback `Alice.credit`.

- `Alice` withdraws another `500_000` cycles to wipe out her savings.

- `Alice` tries to deposit `2_000_000_000` cycles into `porky` but this exceeds `porky`'s capacity by half, so `porky` accepts `1_000_000_000` and refunds the remaining `1_000_000_000` to `Alice`. `Alice` verifies the refund amount (`Cycles.refunded()`), which has been automatically restored to her balance. She also verifies `porky`'s adjusted savings.

- `Alice`'s `credit()` function simply accepts all available cycles by calling `Cycles.accept<system>(available)`, checking the actually `accepted` amount with an assert.

:::note

For this example, Alice is using her readily available cycles that she already owns.

:::

:::danger

Because `porky` consumes cycles in its operation, it is possible for `porky` to spend some or even all of Alice’s cycle savings before she has a chance to retrieve them.

:::

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />

