# Unconditionl loops

An unconditional loop is a loop that runs indefinitely unless explicitly stopped. Unlike `while` or `for` loops, which depend on a condition, unconditional loops continue execution without a predefined exit condition. These loops are useful when a process needs to run continuously, waiting for an external event or relying on an internal break condition.

## Loop

The `loop` keyword explicitly defines an infinite loop. However, when using break, the loop must have a label to indicate where execution should stop.

```motoko
public func unconditionalLoop() : async Nat {
    var count : Nat = 0;

    label countLoop loop {
        count += 1;
        if (count == 5) {
            break countLoop;
        };
    };

    return count;
}
```
