# Contolling program flows

In Motoko, code follows sequential execution, running one statement after another. However, certain constructs allow altering this flow, including exiting a block early, skipping part of a loop, returning a value from a function or calling a function.

## Labels

A label assigns a name to a block of code, allowing structured control flow. This named block executes like any other, but its result can be accessed directly. Labels enable more control over execution, making it possible to define clear exit points and structure complex logic effectively.

When a labeled block runs, it evaluates its contents and produces a result. If no specific result is required, it defaults to an empty value. Labels do not alter how a block executes but provide a way to reference and control its flow.

```motoko
public func labelControlFlow() : async Int {
    label processNumbers : Int {
      let numbers : [Int] = [3, -1, 0, 5, -2, 7];
      var sum : Int = 0;

      for (number in numbers.vals()) {
        sum += number
      };
      sum // The final result of the block
    }
}
```

## Break

A break statement stops execution inside a labeled block and returns a value immediately. However, break must always reference an identifier cannot be used on its own. A labeled block is required to define an exit point.

```motoko
public func breakControlFlow() : async Int {
    label processNumbers: Int {
        let numbers : [Int] = [3, -1, 0, 5, -2, 7];
        var sum : Int = 0;

        for (num in numbers.vals()) {
            if (num < 0) {
                break processNumbers sum; // Exits early with current sum
            };
            sum += num;
        };
        sum // This is skipped if break is triggered
    };
}
```

An identifier must be provided when using the break keyword.

## Continue

A continue statement skips the rest of the current iteration in a loop and moves directly to the next one. Like break, continue must reference a label. It works only within a **labeled loop**, ensuring controlled iteration.

```motoko
public func continueControlFlow() : async Int {
    label processNumbers: Int {
        let numbers : [Int] = [3, -1, 0, 5, -2, 7];
        var sum : Int = 0;

        label processing for (num in numbers.vals()) {
            if (num < 0) {
                continue processing; // Skip negative numbers
            };
            sum += num;
        };
        sum
    };
}
```

`processNumbers` is a labeled block while `processing` is a labeled loop. The continue keyword can only be used in the context of a labeled loop.

## Return

The return statement immediately exits a function and provides a result. Unlike break or continue, which control flow within blocks or loops, return stops execution entirely and hands back a value to the caller.

```motoko
public func returnControlFlow() : async Int {
    let numbers : [Int] = [3, 0, 5, -1, -2, 7];
    var sum : Int = 0;
        for (num in numbers.vals()) {
            if (num < 0) {
                return sum; // Immediately exit the function
            };
            sum += num;
        };
    return sum; // Only reached if no negative numbers are found
}
```

## Function calls

A function call executes a function by passing arguments and receiving a result. In Motoko, function calls may involve synchronous execution within a canister or asynchronous messaging between canisters.  

```motoko
public func processNumbers(numbers: [Int]) : Int {
    var sum : Int = 0;
        for (num in numbers.vals()) {
            if (num < 0) {
                return sum;
            };
            sum += num;
        };
    return sum;
}

public func functionCallControlFlow() : async Int {
    let numbers : [Int] = [3, 1, 5, -1, -2, 7];
    return processNumbers(numbers); // Function call
}
```

Execution begins in `functionCallExample()`, where the function `processNumbers()` is invoked, shifting control to its execution. Within `processNumbers()`, numbers are processed sequentially. If a negative number is encountered, `return` halts execution immediately and returns the current sum. Control then transfers back to `functionCallExample()`, which receives and returns the final result. Function calls disrupt the normal sequential flow by directing execution to a separate block of logic.
