---
sidebar_position: 5
hide_table_of_contents: true
---

# Switch

A switch expression is a selection control structure used for [pattern matching](https://internetcomputer.org/docs/motoko/fundamentals/pattern-matching). They can be used to evaluate a value and execute a corresponding block of code based on the match. It simplifies multiple conditional checks, making the code more readable and efficient by selecting one of several possible execution paths.

A `switch` statement simplifies conditional checks by replacing multiple `if-else` statements. It only executes if the passed argument matches a value, and that value corresponds to a defined case. Importantly, the order of cases matters. If `_` appears first, it will match any input and prevent further cases from being evaluated. Execution stops as soon as a match is found, making `switch` a structured and efficient way to handle multiple conditions.

```motoko no-repl
public func getDayOfWeek(day: Nat) : async Text {
    switch (day) {
        case (1) "Monday";
        case (2) "Tuesday";
        case (3) "Wednesday";
        case (4) "Thursday";
        case (5) "Friday";
        case (6) "Saturday";
        case (7) "Sunday";
        case _ "Invalid day"; // Default case for numbers outside 1-7
    }
}
```

This example function takes a number (`day: Nat`) as input, representing a day of the week. The `switch` expression then compares this number against predefined cases. If a matching case is found, the corresponding day name is returned. If the input does not match any case, such as `0` or `8`, the default case (`case _`) prevents an error (`trap`) by returning `"Invalid day"`.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />