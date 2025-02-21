---
sidebar_position: 5
---

---
sidebar_position: 5
---

# While loop

A `while` loop in Motoko follows this structure:  

```motoko
while (condition) {
  // Code to run while the condition is true
}
```

1. The condition must be a `Bool` (`true` or `false`).  
2. The loop body must have type `()` (it doesn't return a meaningful value).  
3. The loop first evaluates the condition:
   - If it causes a trap (error), the loop stops immediately.  
   - If it evaluates to false, the loop ends and does nothing.  
   - If it evaluates to true, the body runs and the loop repeats the process.  
4. Once the condition becomes false, the loop stops, and the final result is `()`.

```motoko
var count = 3;
while (count > 0) {
  Debug.print("Counting down...");
  count -= 1;
};
```
