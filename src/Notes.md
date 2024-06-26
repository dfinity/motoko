>TODO and Issues

- Address TBR and TBD comments in source.
- Why does BinE not take two immediate sub-expressions.  Currently it only has one so the parser has to use TupE to create a tuple. A: this is a typo, make it binary.
- Relational operators (=, \<, etc.) are missing - just treat them as binops? Yes, or add another class of bool producing RelE operations
- Lhs of AssignE ast node is too general - these should be *locations* - shall we encode this
  in the syntax or as a static check? Preferably as static check - note we should allow array locations and object fields as l-values too. Alternative: introduce second class address ypes, perhaps indexed by location type for type-directed compilation.
- what syntax do we want for rotation operators - do we want to support JS \>\>\> operator?
  Andreas has seen something like \<\>\> used in other languages.a
- do we really want async as a firs-class value or should we
  syntactically restrict await to async function calls and immediate blocks?
  The former supports overlapped io, passing async's as values and the registration of multiple waiters on the same async value.
  The second dissallows overlapped calls and allows exactly one waiter to await an async call.
- it looks like we want operator overloading, at least on primitive types. Is that correct? Yes
- do we want conversion (eg. int32-\>iny64), both implicit and explicit. No, just use
  functions for now.
- the parser requires parens around the condition of a while (...) ...  and loop ... while (...) constructs. This appears to agree with JS. A: Andreas would like a more principled solution using expression based syntax, while atomic_exp atomic exp should do.
- unary update (++i etc) NYI but should be - not sure about postfix variants
- I had to introduce a params poductions ensure params are comma separated sequences of typed identifiers, not just an arbitrary pat. Andreas: intentions was to have static restriction that all identifiers in pat are typed, either individually or by outer annotation.
- do we want n-ary argument function types? Yes.
- explicit type application is missing. We should add it.
- We should use some method to annotate typed expressions with their types (see eg. Hamlet)
- switch patterns are useless without literal patterns. I'll just add them

- we should really take lubs across control flow branches.
- is Null really a type or a null just a value of type `T?` for any T. Option okay for now.
- TODO change syntax option T to T?
- can you label any expression or just loops - if any expression, any of what type?
- is for just for arrays? Does iterating over a mutable array bind a mutable value?
- how can we create an immutable array of arbitrary size - with a primitive?
- add keywords for primitive types (uppercase?)
- what subtyping do we want, just to and between like types?
- add switch on option type?
- what's the intuition for break es when |es| > 0: is this to break from a block or switch that returns a n-value (or just a tuple). Yes. Single exp would do in ast.a
- do we want blocks to return the last value in the block, of any type; Yes
= do we want intermediate statements in a block to have any type or unit type. Unit type
- are we using TupT([]) as unit type? yes
- should BreakE and ContE be polymorphic or just unit typed. Polymorphic.

- how will we check IsE(e,t) at run-time if t contains type parameters (in the absence of type passing)
- do we want to distinguish between awaitable 'async ()' and unawaitable 'async' (C# has both I believe, the latter for true fire-and-forget asyncs)
- why do we have a Nat type? What's wrong with (unsigned word)

- operators:
   - do we want to allow implict promotion of operands (ie word16+word8) No
   - do we want to allow logical operation (bitwise operators) on non-word types? No

- there's a parsing ambiguity between infix application, indexing and array literals is e [] indexing or application to an array literal.

- we need to discuss the semantics and typing of while/loop/loop-while and for. Should the bodies by unit typed or polymorphic?

- since we have a notion of (non)-shareable type do we want a notion of sharable kind so we can abstract over one or the other?
- the transfer function in the README is wrong receiver.join(trx) should be trx.join(receiver).

Claudio's TODOS:
- we should add a PrimE form for primitive functions.
- all the prim ops are dodgy and don't check for overflow etc.
- introduce tables for the operators,types and implementations and then annotate operators with their implementations to avoid redoing the dispatch at interp time.
- type tests either need type-passing or unpack like parameteric semantics (for erasure)
- Interpreter: it would be better to distinguish statically between bindings (RecV and VarV) and values
- local decs are not mutually recursive and scoped sequentially - relax this.
- what's the semantics of a message send to self? Do we go through the scheduler or short-cut and execute synchronously as an ordinary call?
- prevent escape of locally defined types