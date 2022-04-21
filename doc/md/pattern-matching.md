# Pattern matching

Pattern matching is a language feature that makes it easy to both test and decompose structured data into its constituent parts. While most programming languages provide familiar ways to build structured data, pattern matching enables you to take apart structured data and bring its fragments into scope by binding them to the names you specify. Syntactically, the patterns resemble the construction of structured data, but generally appear in input-direction positions, such as in function argument positions, after the `case` keyword in `switch` expressions, and after `let` or `var` declarations.

Consider the following function call:

``` motoko
let name : Text = fullName({ first = "Jane"; mid = "M"; last = "Doe" });
```

This code constructs a record with three fields and passes it to the function `fullName`. The result of the call is named and brought into scope by binding it to the identifier `name`. The last, binding step is called pattern matching, and `name : Text` is one of the simplest forms of pattern. For instance, in the following implementation of the callee:

``` motoko
func fullName({ first : Text; mid : Text; last : Text }) : Text {
  first # " " # mid # " " # last
};
```

The input is an (anonymous) object, which is destructured into its three `Text` fields, whose values are bound to the identifiers `first`, `mid` and `last`. They can be freely used in the block that forms the body of the function. Above we have resorted to *name punning* (a form of aliasing) for object field patterns, using the name of a field to also name its contents. A more general form of field pattern allows the content to be named separately from the field, as in `…​; mid = m : Text; …​`. Here `mid` determines which field to match, and `m` names the content of that field within the scope of the pattern.

You can also use pattern matching to declare *literal patterns*, which look just like literal constants. Literal patterns are especially useful in `switch` expressions because they can cause the current pattern match to *fail*, and thus start to match the next pattern. For example:

``` motoko
switch ("Adrienne", #female) {
  case (name, #female) { name # " is a girl!" };
  case (name, #male) { name # " is a boy!" };
  case (name, _) { name # ", is a human!" };
}
```

1.  will match the first `case` clause (because binding to the identifier `name` cannot fail and the shorthand variant literal `#Female` compares as equal), and evaluate to `"Adrienne is a girl!"`. The last clause showcases the *wildcard* pattern `_`. It cannot fail, but won’t bind any identifier.

The last kind of pattern is the `or` pattern. As its name suggests, these are two or more patterns that are separated by the keyword `or`. Each of the sub-patterns must bind to the same set of identifiers, and is matched from left-to-right. An `or` pattern fails when its rightmost sub-pattern fails.

| pattern kind               | example(s)                      | context    | can fail                              | remarks                                  |
|----------------------------|---------------------------------|------------|---------------------------------------|------------------------------------------|
| literal                    | `null`, `42`, `()`, `"Hi"`      | everywhere | when the type has more than one value |                                          |
| named                      | `age`, `x`                      | everywhere | no                                    | introduces identifiers into a new scope  |
| wildcard                   | `_`                             | everywhere | no                                    |                                          |
| typed                      | `age : Nat`                     | everywhere | conditional                           |                                          |
| option                     | `?0`, `?val`                    | everywhere | yes                                   |                                          |
| tuple                      | `( component0, component1, …​ )` | everywhere | conditional                           | must have at least two components        |
| object                     | `{ fieldA; fieldB; …​ }`         | everywhere | conditional                           | allowed to mention a subset of fields    |
| field                      | `age`, `count = 0`              | object     | conditional                           | `age` is short for `age = age`           |
| variant                    | `#celsius deg`, `#sunday`       | everywhere | yes                                   | `#sunday` is short form for `#sunday ()` |
| alternative (`or`-pattern) | `0 or 1`                        | everywhere | depends                               | no alternative may bind an identifier    |

The following table summarises the different ways of pattern matching.

## Additional information about about patterns

Since pattern matching has a rich history and interesting mechanics, a few additional comments are justified.

terminology  
The (usually structured) expression that is being matched is frequently called the *scrutinee* and the patterns appearing behind the keyword `case` are the *alternatives*. When every possible scrutinee is matched by (at least one) alternative, then we say that the scrutinee is *covered*. The patterns are tried in top-down fashion and thus in case of *overlapping* patterns the one higher-up is selected. An alternative is considered *dead* (or *inactive*), if for every value that it matches there is higher-up alternative that is also matched.

booleans  
The data type `Bool` can be regarded as two disjointed altenatives (`true` and `false`) and Motoko’s built-in `if` construct will *eliminate* the data and turn it into *control* flow. `if` expressions are a form of pattern matching that abbreviates the general `switch` expression for the special case of boolean scrutinees.

variant patterns  
Motoko’s variant types are a form of *disjoint union* (sometimes also called a *sum type*). A value of variant type always has exactly one *discriminator* and a payload which can vary from discriminator to discriminator. When matching a variant pattern with a variant value, the discriminators must be the same (in order to select the alternative) and if so, the payload gets exposed for further matching.

enumerated types  
Other programming languages — for example C, but not Motoko — often use a keyword `enum` to introduce enumerations. These are impoverished relatives of Motoko’s variant types, as the alternatives are not allowed to carry any payload. Correspondingly, in those languages the `switch`-like statements lack the full power of pattern matching. Motoko provides the short-hand syntax (as in `type Weekday = { #mon; #tue; …​ }`) to define basic enumerations, for which no payloads are required.

error handling  
Error handling can be considered a use-case for pattern matching. When a function returns a value that has an alternative for success and one for failure (for example, an option value or a variant), pattern matching can be used to distinguish between the two as discussed in [Error handling](errors.md).

irrefutable matching  
Some types contain just a single value. We call these *singleton types*. Examples of these are the unit type (also known as an empty tuple) or tuples of singleton types. Variants with a single tag and no (or singleton-typed) payload are singleton types too. Pattern matching on singleton types is particularly straightforward, as it only has one possible outcome: a successful match.

exhaustiveness (coverage) checking  
When a pattern check alternative has the potential to fail, then it becomes important to find out whether the whole `switch` expression can fail. If this can happen the execution of the program can trap for certain inputs, posing an operational threat. To this end, the compiler checks for the exhaustiveness of pattern matching by keeping track of the covered shape of the scrutinee. The compiler issues a warning for any non-covered scrutinees (Motoko even constructs a helpful example of a scrutinee that is not matched). A useful by-product of the exhaustiveness check is that it identifies and warns about dead alternatives that can never be matched.

In summary, pattern checking is a great tool with several use-cases. By statically analyzing patterns, the compiler assists the programmer by pointing out unhandled cases and unreachable code, both of which often indicate programmer error. The static, compile-time nature of coverage checking reliably rules out runtime failures.
