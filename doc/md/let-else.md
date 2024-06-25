# Providing alternate control flow for pattern match failures

The most general pattern matching construct is `switch`, and `if-else` is a special case of it when the value that we match on is a `Bool`. Similarly, one can consider `let` with a pattern to be a special `switch` with one arm and which is trapping when that match fails. In this section we'll describe the `let-else` construct, which allows one to customise the failure mode.

Consider a function written as follows:

``` motoko
func binaryDigit(digit : Char) : ?Nat32 {
  let '0' or '1' = digit else return null;
  ?(Char.toNat32 digit - Char.toNat32 '0')
};
```

Here the first thing the function does is to test its argument against a pattern and return early when the
matching fails. In the opposite case the result computation can proceed.

Note that writing the function in this style doesn't nest the handling of the failure case into one arm of
a `switch` expression, which can sometimes improve readability by eliminating the so-called
(_pyramid of doom_)[https://en.wikipedia.org/wiki/Pyramid_of_doom_(programming)].

Naturally, all other features (not shown above) of the pattern-matiching `let` declaration are available in `let-else`,
such as destructuring of values and binding of several variables, but the `else` portion is only sensible when the pattern
matching is _refutable_ (i.e. can fail).

A special condition the `else` part must satisfy is that it must change the control flow of the execution sequence,
i.e. it must divert the execution from arriving at code that syntactically follows the `let`. In terms of the type system
this condition is checked by the requirement that the expression constituting the `else` part must have type `None`. There
is a great variety of expressions that fall into this category, such as jump-like expressions (`throw`, `break`, `continue`),
function `return`, and function calls with `None`-typed result (i.e. calls to functions that never return a value).

See [here](language-manual.md#handling-pattern-match-failures) for more details.
