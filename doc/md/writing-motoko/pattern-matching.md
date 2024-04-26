---
sidebar_position: 19
---

# Pattern matching

## Overview

Pattern matching is a language feature that makes it easy to both test and decompose structured data into its constituent parts. While most programming languages provide familiar ways to build structured data, pattern matching enables you to take apart structured data and bring its fragments into scope by binding them to the names you specify. Syntactically, the patterns resemble the construction of structured data, but generally appear in input-direction positions, such as in function argument positions, after the `case` keyword in `switch` expressions, and after `let` or `var` declarations.

## Types of patterns

The following table summarizes the different ways of pattern matching.

| Pattern kind               | Example(s)                      | Context    | Can fail                              | Remarks                                  |
|----------------------------|---------------------------------|------------|---------------------------------------|------------------------------------------|
| Literal                    | `null`, `42`, `()`, `"Hi"`      | Everywhere | When the type has more than one value |                                          |
| Named                      | `age`, `x`                      | Everywhere | No                                    | Introduces identifiers into a new scope  |
| Wildcard                   | `_`                             | Everywhere | No                                    |                                          |
| Typed                      | `age : Nat`                     | Everywhere | Conditional                           |                                          |
| Option                     | `?0`, `?val`                    | Everywhere | Yes                                   |                                          |
| Tuple                      | `( component0, component1, …​ )` | Everywhere | Conditional                           | Must have at least two components        |
| Object                     | `{ fieldA; fieldB; …​ }`         | Everywhere | Conditional                           | Allowed to mention a subset of fields    |
| Field                      | `age`, `count = 0`              | Object     | Conditional                           | `age` is short for `age = age`           |
| Variant                    | `#celsius deg`, `#sunday`       | Everywhere | Yes                                   | `#sunday` is short form for `#sunday ()` |
| Alternative (`or`-pattern) | `0 or 1`                        | Everywhere | Depends                               | No alternative may bind an identifier    |



## Using pattern matching

Consider the following function call:

``` motoko include=fullname
let name : Text = fullName({ first = "Jane"; mid = "M"; last = "Doe" });
```

This code constructs a record with three fields and passes it to the function `fullName`. The result of the call is named and brought into scope by binding it to the identifier `name`. The last, binding step is called pattern matching, and `name : Text` is one of the simplest forms of a pattern. For instance, in the following implementation of the callee:

``` motoko name=fullname
func fullName({ first : Text; mid : Text; last : Text }) : Text {
  first # " " # mid # " " # last
};
```

The input is an anonymous object which is destructured into its three [`Text`](../base/Text.md) fields, whose values are bound to the identifiers `first`, `mid` and `last`. They can be freely used in the block that forms the body of the function. Above we have resorted to name punning, a form of aliasing for object field patterns, using the name of a field to also name its contents. A more general form of field patterns allows the content to be named separately from the field, as in `…​; mid = m : Text; …​`. Here `mid` determines which field to match, and `m` names the content of that field within the scope of the pattern.

## Literal patterns

You can also use pattern matching to declare literal patterns, which look just like literal constants. Literal patterns are especially useful in `switch` expressions because they can cause the current pattern match to fail, and thus start to match the next pattern. For example:

``` motoko
switch ("Adrienne", #female) {
  case (name, #female) { name # " is a girl!" };
  case (name, #male) { name # " is a boy!" };
  case (name, _) { name # ", is a human!" };
}
```

This program will match the first `case` clause because binding to the identifier `name` cannot fail and the shorthand variant literal `#female` compares as equal. Then it evaluates to `"Adrienne is a girl!"`. The last clause showcases the wildcard pattern `_`. It cannot fail, but won’t bind any identifier.

## `or` patterns

The last kind of pattern is the `or` pattern. As its name suggests, these are two or more patterns that are separated by the keyword `or`. Each of the sub-patterns must bind to the same set of identifiers, and is matched from left-to-right. An `or` pattern fails when its rightmost sub-pattern fails.

## More on patterns

Since pattern matching has a rich history and interesting mechanics, a few additional comments are justified.

### Terminology


The expression whose value is being matched is frequently called the **scrutinee**. The patterns appearing behind the keyword `case` are the **alternatives**. When every possible value of the scrutinee is matched by at least one alternative, then the scrutinee is **covered**. The alternatives are tried in order. In case of overlapping patterns, the earlier one is selected. An alternative is considered dead or redundant if for every value that it matches there is already some earlier alternative that also matches the value.

### Booleans

The data type [`Bool`](../base/Bool.md) can be regarded as two disjointed alternatives (`true` and `false`) and Motoko’s built-in `if` construct will eliminate the data and turn it into control flow. `if` expressions are a form of pattern matching that abbreviates the general `switch` expression for the special case of boolean scrutinees.

### Variant patterns

Motoko’s variant types are a form of disjoint union, sometimes also called a sum type. A value of variant type always has exactly one discriminator and a payload which can vary from discriminator to discriminator. When matching a variant pattern with a variant value, the discriminators must be the same in order to select the alternative, and if so, the payload gets exposed for further matching.

### Enumerated types

Other programming languages often use a keyword `enum` to define discrete enumerations. These are poor relations of Motoko’s more general variant types, as the alternatives of an enumeration are not allowed to carry any payload. Correspondingly, in those languages the `switch`-like statements used to analyse enum values lack the full power of pattern matching. Motoko provides the short-hand syntax, as in `type Weekday = { #mon; #tue; …​ }`, to define basic enumerations for which no payloads are required.

### Error handling

Error handling can be considered a use-case for pattern matching. When a function returns a value that has an alternative for success and one for failure, pattern matching can be used to distinguish between the two as discussed in [error handling](errors.md).

### Irrefutable patterns

A pattern is refutable if matching some value of the expected type against it can fail. Literal and variant patterns are generally refutable, since they require an equal value or variant tag and these could fail to match.

A pattern that cannot fail to match every value is irrefutable. Examples of irrefutable patterns are the wildcard pattern `_`, identifier patterns `x` and tuple or record patterns built from irrefutable sub-patterns.

Some types contain just a single value. We call these singleton types. Examples of these are the unit type, also known as an empty tuple, or tuples of singleton types. Variants with a single tag and with no or a singleton type payload are singleton types too. Pattern matching on singleton types is particularly straightforward, as it only has one possible outcome of a successful match.

### Exhaustiveness (coverage) checking

When a pattern check alternative has the potential to fail, then it becomes important to find out whether the whole `switch` expression can fail. If this can happen, the execution of the program can trap for certain inputs, posing an operational threat. The compiler checks for the exhaustiveness of pattern matching by keeping track of the covered shape of the scrutinee. The compiler issues a warning for any non-covered scrutinees. Motoko even constructs a helpful example of a scrutinee that is not matched. A useful byproduct of the exhaustiveness check is that it identifies and warns about dead alternatives that can never be matched.