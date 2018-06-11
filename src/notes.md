TODO

- Address TBR and TBD comments in source.
- Why does BinE not take two immediate sub-expressions.  Currently it only has one so the parser has to use TupE to create a tuple.
- Relational operators (=, <, etc.) are missing - just treat them as binops?
- Lhs of AssignE ast node is too general - these should be *locations* - shall we encode this
  in the syntax or as a static check?
- what syntax do we want for rotation operators - do we want to support JS >>> operator?
- do we really want async as a firs-class value or should we
  syntactically restrict await to async function calls and immediate blocks?
  The former supports overlapped io, passing async's as values and the registration of multiple waiters on the same async value.
  The second dissallows overlapped calls and allows exactly one waiter to await an async call.
- it looks like we want operator overloading, at least on primitive types. Is that correct?
- do we want conversion (eg. int32->iny64), both implicit and explicit.
- the parser requires parens around the condition of a while (...) ...  and loop ... while (...) constructs. This appears to agree with JS.
- are we encoding primitive types as nullary type applications of well-known identifiers or should we bake them in with dedicated keywords and new ast nodes.
- unary update (++i etc) NYI
- I had to introduce a params poductions ensure params are comma separated sequences of typed identifiers, not just an arbitrary pat.
- do we want n-ary argument function types?

