# Control flow

Control flow determines the order in which a program executes its instructions. It directs the path of execution based on conditions and decisions.

By default, code follows **sequential execution**, running line by line. **Conditional statements** allow the program to execute different code based on conditions, **loops** (for, while) repeatedly execute a block of code as long as a condition remains true. **Function calls** enable the program to jump to a separate block of code to perform a specific task before returning to the original execution point.

## While loops

A `while` loop in Motoko follows this structure:  

```motoko
while (condition) {
  // Code to run while the condition is true
}
```

1. The condition must be a `Bool` (`true` or `false`).  
2. The loop body must have type `()` (it doesn't return a meaningful value).  
3. The loop first evaluates the condition:
   - If it causes a **trap** (error), the loop **stops** immediately.  
   - If it evaluates to **false**, the loop **ends** and does nothing.  
   - If it evaluates to **true**, the body runs and the loop **repeats** the process.  
4. Once the condition becomes **false**, the loop **stops**, and the final result is `()`.

```motoko
var count = 3;
while (count > 0) {
  Debug.print("Counting down...");
  count -= 1;
};
```

