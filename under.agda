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
  ident : ∀ (v : Bool) → Token 0 false v false 0
  empty : Token 0 false false false 0
  prefix : ∀ {pre post : Nat} {r v n : Bool} → (pref : Nat) → Token pre r v n post → Token (pref + pre) false v false post
  suffix : ∀ {pre post : Nat} {r v n : Bool} → Token pre r v n post → (suf : Nat) → Token pre false v false (suf + post)


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
