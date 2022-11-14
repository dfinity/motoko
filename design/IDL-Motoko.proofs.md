Proofs related to IDL-Motoko.md
===============================

Field name escaping
-------------------

### Assumptions

```
isReserved : String -> Bool
isValid : String -> Bool

A1: ∀s. isReserved s -> isValid s
A2: ∀s. ¬isReserved (s "_")
A3: ∀s. isValid s -> isValid (s "_")
A4: ∀n. isValid ("_" n "_")
A5: ∀s. ¬isReserved ("_" s)
A6: ∀n. ¬isValid ("")
```

### Definitions

```
escape_number <nat> = "_" <nat> "_"

// IDL name to Motoko
escape : <name> -> <id>
escape <name> = <name> "_"  if <name> is a reserved identifier in Motoko
escape <name> = <name> "_"  if <name> is a valid Motoko <id> ending in "_"
escape <name> = <name>  if <name> is a valid Motoko <id> not ending in "_"
escape <name> = escape_number(hash(<name>))   otherwise

// Motoko to IDL
unescape : <id> -> <nat>|<name>
unescape("_" <nat> "_") = <nat>
unescape(<id> "_") = <id>
unescape(<id>) = <id>
```

### Properties

**Prop 1**: The range of escape is in the set of valid Motoko field names.
`∀ s. isValid(escape(s)) ∧ ¬ isReserved(escape(s))`

**Proof**: Case analysis according to the definition of `escape`:
1. If `s` is a reserved identifier, then `isValid(s)` by A1 and `isValid(escape(s))` by A3, and also `¬ isReserved(escape(s))` by A2.
2. If `isValid(s)` and `s` ends in `"_"`, then `isValid(escape(s))` by A3 and `¬ isReserved(escape(s))` by A2.
3. If `isValid(s)` and `¬ isReserved(escape(s))`, then the thesis follows from `escape(s) = s`.
4. by A4 and A2

**Prop 2**: `∀s1 s2, escape(s1) = escape(s2) → hash(s1) = hash(s2)`
**Proof**: The four equations of `escape` have disjoint ranges: Only equation 3 produces output not ending in `"_"`, only equation 4 produces output beginning in `"_"` and ending in exactly one `"_"` (due to A6, A1), only equation 1 produces output beginning with no `"_"` and ending with exactly one `"_"` (due to A2, A5, A6), only equation 2 produces output ending in more than one `"_"`.

Therefore it suffices to look at each equation separately. Equations 1-3 are injective, and equation 4 is injective up-to `hash(…)`, because `escape_number` is injective.

**Prop 3**: If `unescape(escape_number(n)) = m:nat`, then `n = m`.

**Proof**: By definition.

**Prop 4**: If `unescape(escape_number(n)) = i:id`, then `hash(i) = n`.

**Proof**: The assumption is never true, by definition.

**Prop 5**: If `unescape(escape(i)) = m:nat`, then `hash(i) = m`.

**Proof**: By construction of `escape`, `unescape(escape(i))` is only a number in the fourth case of `i`, so we have `m = unescape(escape_number(hash(i))) = hash(i)`.

**Prop 6**: If `unescape(escape(i1)) = i2:id`, then `hash(i1) = hash(i2)`.

**Proof**: By cases on `escape`.
1. `escape(i1)` does not match the first equation of `unescape` (by A5), but the second, so we have `i2 = unescape(i1 "_") = i1`.
2. `escape(i1)` does not match the first equation of `unescape` (by construction), but the second, so we have `i2 = unescape(i1 "_") = i1`.
3. `escape(i1)` does not match the first or second equation of `unescape` (by construction), but the second, so we have `i2 = unescape(i1) = i1`.
3. `escape(i1)` matches the first equation of `unescape`, so the output is a number, not an id, so the assumption of the proposition is false.

**Prop 7**: The range of `unescape` contains all `<nat>`

**Proof**: For all `n`, `unescape("_" n "_") = n`.
