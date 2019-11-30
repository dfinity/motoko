{-data Nat : Set where
  Z : Nat
  S : Nat → Nat
# BUILTIN NATURAL Nat #-}

open import Agda.Builtin.String
open import Agda.Builtin.Bool
open import Agda.Builtin.Nat


data Lang : Set where
  IDL Motoko : Lang

--                        +-- reserved
--                        |
--                        |      +-- valid
--                        |      |
--                        |      |      +-- num
--                        v      v      v
data Token : Lang → Nat → Bool → Bool → Bool → Nat → Set where
  -- stems
  keyword : Token IDL 0 true true false 0
  number : Token IDL 0 false false true 0
  _ident : ∀ (v : Bool) → Token IDL 0 false v false 0 -- TODO: post should be supplied
  empty : Token IDL 0 false false false 0
  -- wrappers
  suffix : ∀ {pre post : Nat} {r : Bool} → Token IDL pre r true false post → Token Motoko pre false true false (suc post)
  escape-number : Token IDL 0 false false true 0 → Token Motoko 1 false true false 1
  hash : ∀ {pre post : Nat} → Token IDL pre false false false post → Token IDL 0 false false true 0
  side : ∀ {pre post : Nat} → Token IDL pre false true false post → Token Motoko pre false true false post


if_then_else_ : {A : Set} → Bool → A → A → A
if true then x else y = x
if false then x else y = y

not : Bool → Bool
not false = true
not true = false

_and_ : Bool → Bool → Bool
true and b = b
_ and _ = false

_or_ : Bool → Bool → Bool
false or b = b
_ or _ = true

escape : ∀ {pre post : Nat} {r v n : Bool} → Token IDL pre r v n post → Token Motoko (if (n or (not v)) then 1 else 0) false true false (if v and not r then 0 else 1)
escape k@keyword = suffix k
escape n@number = escape-number n
escape i@(false ident) = escape-number (hash i)
escape i@(true ident) = side i
escape e@empty = escape-number (hash e)
-- escape s@(suffix t) = {! s  !}
-- escape (escape-number t) = {!   !}
escape h@(hash t) = escape-number h


unescape : ∀ {pre post : Nat} {r v n : Bool} → Token Motoko pre r v n post → Token IDL pre r v n post
unescape (suffix t) = {!   !}
unescape (escape-number t) = {!   !}
unescape (side t) = t


-- field N {Empty|Valid|Reserved|Num|Invalid} N

{-- implication isReserved -> isValid
postulate isReserved isNum : String → Bool
isValid : String → Bool
isValid i with isReserved i
... | true = true
isValid _ = false
-}
