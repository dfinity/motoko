{-data Nat : Set where
  Z : Nat
  S : Nat → Nat
# BUILTIN NATURAL Nat #-}

open import Agda.Builtin.String
open import Agda.Builtin.Bool
open import Agda.Builtin.Nat

--                 +-- reserved
--                 |
--                 |      +-- valid
--                 |      |
--                 |      |      +-- num
--                 v      v      v
data Token : Nat → Bool → Bool → Bool → Nat → Set where
  -- stems
  keyword : Token 0 true true false 0
  number : Token 0 false false true 0
  _ident : ∀ (v : Bool) → Token 0 false v false 0 -- TODO: post should be supplied
  empty : Token 0 false false false 0
  -- wrappers
  suffix : ∀ {pre post : Nat} {r : Bool} → Token pre r true false post → Token pre false true false (suc post)
  escape-number : Token 0 false false true 0 → Token 1 false true false 1
  hash : ∀ {pre post : Nat} → Token pre false false false post → Token 0 false false true 0


if_then_else_ : {A : Set} → Bool → A → A → A
if true then x else y = x
if false then x else y = y

escape : ∀ {pre post : Nat} {r v n : Bool} → Token pre r v n post → Token (if n then 1 else 0) false true false 1
escape k@keyword = suffix k
escape n@number = escape-number n
escape (false ident) = {! escape-number (hash f)  !}
escape (true ident) = {!   !}
escape empty = {!   !}
escape (suffix t) = {!   !}
escape (escape-number t) = {!   !}
escape (hash t) = {!   !}


-- escape : Ident → Ident
-- escape i = i

-- unescape : ∀ {pre post : Nat} → pre Ident post → pre Ident post
-- unescape keyword


-- field N {Empty|Valid|Reserved|Num|Invalid} N

{-- implication isReserved -> isValid
postulate isReserved isNum : String → Bool
isValid : String → Bool
isValid i with isReserved i
... | true = true
isValid _ = false
-}
