# System Capability

The `system` capability in Motoko is used to control access to sensitive system functions and prevent potential misuse of these functions.
It serves as a safety mechanism to ensure that developers are explicitly aware when they are using or granting access to powerful system-level operations.

Specifically, the system capability is required for calling sensitive functions such as:

1. `ExperimentalCycles.add(...)`: This function is used for cycle management, which is crucial for controlling the computational resources on the Internet Computer.

2. `Timer.setTimer(...)`: This function is used for scheduling future computations, which can have significant impacts on system resources and behavior.

The introduction of the system capability helps address a security concern where, in previous versions of Motoko, third-party library functions could make silent calls
to these sensitive functions without providing any indication to the caller.
This could potentially lead to unexpected behavior or resource usage.

By requiring explicit declaration and passing of the system capability, Motoko now ensures that developers are fully aware when they are using or allowing the use of these powerful system functions.
This helps prevent accidental misuse and makes the code's intentions clearer, enhancing both security and code readability.

It's important to note that while the system capability allows the use of these sensitive functions, it doesn't automatically grant unlimited access to all system resources.
It's a type-level mechanism to control and make explicit the use of specific system functions, rather than a comprehensive permission system.

The system capability was introduced in Motoko version 0.11.0.
The key change is the introduction of the 'system' pseudo-type parameter and argument.
For instance, `ExperimentalCycles.add` has been revised from having type `Nat -> ()` to having type `<system>Nat -> ()`,
reflecting the new system capability requirement.

To use system capabilities in your code, you must now explicitly declare them.
Functions that need to use system capabilities must include the 'system' pseudo-type parameter.
It's important to note that 'system', if specified, must be the first parameter in function or class declarations.

For example:

```motoko no-repl
func splitCycles<system>() {
  let amount = ExperimentalCycles.balance() / 2;
  ExperimentalCycles.add(amount);
}
```

To suppress warnings about implicit system capability usage, you can
explicitly pass the 'system' capability at call sites:

```
ExperimentalCycles.add<system>(amount);
```

System capabilities are available in specific contexts, such as within
actor expression or actor class bodies, non-query shared functions,
asynchronous functions, async expressions, local functions or classes
declared with the 'system' pseudo-type parameter, and the preupgrade
and postupgrade system functions.

However, they are not available in query methods or composite query
methods or functions that don't declare their need of system capability.

When migrating existing code, you may encounter compiler errors for
missing 'system' capabilities. For instance:

```motoko no-repl
func splitCycles() {
  let amount = ExperimentalCycles.balance() / 2;
  ExperimentalCycles.add(amount); // This will now cause an error
}
```
To resolve this, add the 'system' pseudo-type parameter to the function declaration:

```motoko no-repl
func splitCycles<system>() {
  let amount = ExperimentalCycles.balance() / 2;
  ExperimentalCycles.add(amount); // This will work, but with a warning
}
```

The compiler will issue warnings for implicit 'system' capability
usage. To address these, explicitly declare and pass 'system'
capabilities when required.





