(*
This backend module for arbitrary-precision arithmetics defines the interface
for binary data that is representing numbers (in particular ActorScript's
Int and Nat data types. 

Besides the usual arithmetics it supports:
- Boxing / (speculative) unboxing to i32/i64
- Transparent representation change on overflow
- Bounds check (e.g. for array indexing)
- Garbage collection (moving)
- (De)serialization
- Materialise literal
- Match literal pattern

Arithmetic operations

Equality
comparison
Add/Sub/Mul/Div/Pow

Conversions to/from Word*
*)

