TODO and Issues

- Address TBR and TBD comments in source.
- Why does BinE not take two immediate sub-expressions.  Currently it only has one so the parser has to use TupE to create a tuple. A: this is a typo, make it binary.
- Relational operators (=, <, etc.) are missing - just treat them as binops? Yes, or add another class of bool producing RelE operations
- Lhs of AssignE ast node is too general - these should be *locations* - shall we encode this
  in the syntax or as a static check? Preferably as static check - note we should allow array locations and object fields as l-values too. Alternative: introduce second class address ypes, perhaps indexed by location type for type-directed compilation.
- what syntax do we want for rotation operators - do we want to support JS >>> operator?
  Andreas has seen something like <>> used in other languages.a
- do we really want async as a firs-class value or should we
  syntactically restrict await to async function calls and immediate blocks?
  The former supports overlapped io, passing async's as values and the registration of multiple waiters on the same async value.
  The second dissallows overlapped calls and allows exactly one waiter to await an async call.
- it looks like we want operator overloading, at least on primitive types. Is that correct? Yes
- do we want conversion (eg. int32->iny64), both implicit and explicit. No, just use
  functions for now.
- the parser requires parens around the condition of a while (...) ...  and loop ... while (...) constructs. This appears to agree with JS. A: Andreas would like a more principled solution using expression based syntax, while atomic_exp atomic exp should do.
- unary update (++i etc) NYI but should be - not sure about postfix variants
- I had to introduce a params poductions ensure params are comma separated sequences of typed identifiers, not just an arbitrary pat. Andreas: intentions was to have static restriction that all identifiers in pat are typed, either individually or by outer annotation.
- do we want n-ary argument function types? Yes.
- explicit type application is missing. We should add it.
- We should use some method to annotate typed expressions with their types (see eg. Hamlet)