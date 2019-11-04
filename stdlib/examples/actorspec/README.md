# ActorSpec

A testing framework for Motoko.

## Demo

```sh
$ cd dev/sdk/actorspec
$ nix-build demo
```

```
ActorSpec: Failed: 1, Passed: 2, Pending: 1

  passing specs: Failed: 0, Passed: 1, Pending: 0
    should pass: Passed

  failing specs: Failed: 1, Passed: 0, Pending: 0
    should fail: Failed

  pending specs: Failed: 0, Passed: 0, Pending: 1
    should be marked as pending: Pending

  nested groups: Failed: 0, Passed: 1, Pending: 0
    should have the correct indentation: Passed

Failed: 1, Passed: 2, Pending: 1
```
