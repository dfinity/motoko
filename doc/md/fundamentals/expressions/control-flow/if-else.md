# If-else statements and expressions

An if-else expression allows a program to make decisions based on a condition. It evaluates a boolean expression and selects one of two possible execution paths.

```motoko

var age : Nat = 21
  if (age < 18) {
    "You are a minor."
  } else if (age >= 18 and age < 65) {
      "You are an adult."
    } else {
      "You are a senior citizen."
    }
```

## If-expression

```motoko
let identity : Text = if (x == 1) "x is 1" else "x is not 1"; // Produces a value
```

- The result of the `if` block is assigned to `identity`.  
- Both branches must return a matching type (`Text` in this case).  

## If-Statement

```motoko
if (x == 1) {
    Debug.print("x is 1"); // Executes but does not return a value
} else {
    Debug.print("x is not 1");
}
```

- This executes a block of code but does not return a value.  
- It cannot be assigned to a variable since its type is `()`.  

| If-Expression                                        | If-Statement                              |
|------------------------------------------------------|-------------------------------------------|
| Type `T` (must be consistent across branches)              | `()` (does not return a value)           |
| Produces a result that can be used or assigned | Executes a block of code but does not return a value |
| Can be assigned to a variable                 | Executes for side effects only       |
| Used when a decision determines a value       | Used when a decision triggers an action  |
