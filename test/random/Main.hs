{-# language ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, GADTs
           , KindSignatures, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving
           , TypeApplications, TypeOperators, TypeFamilies, TupleSections
           , UndecidableInstances, ViewPatterns #-}

{-# options_ghc -Wno-missing-methods #-}

module Main where

import Control.Applicative
import Control.Monad
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding ((.&.))
import Test.QuickCheck.Unicode
import qualified Data.Text (null, unpack)
import Data.Maybe
import Data.Bool (bool)
import Data.Proxy
import GHC.Natural
import GHC.TypeLits
import qualified Data.Word
import Data.Bits (Bits(..), FiniteBits(..))
import Numeric

import System.Process hiding (proc)
import Turtle
-- import Debug.Trace (traceShowId)

main = defaultMain tests
  where tests :: TestTree
        tests = testGroup "ActorScript tests" [{-arithProps, utf8Props,-} matchingProps]

arithProps = testGroup "Arithmetic/logic"
  [ QC.testProperty "expected failures" $ prop_rejects
  , QC.testProperty "expected successes" $ prop_verifies
  ]


utf8Props = testGroup "UTF-8 coding"
  [ QC.testProperty "explode >>> concat roundtrips" $ prop_explodeConcat
  , QC.testProperty "charToText >>> head roundtrips" $ prop_charToText
  ]

matchingProps = testGroup "pattern matching"
  [ QC.testProperty "intra-actor" $ prop_matchStructured
  --, QC.testProperty "inter-actor" $ prop_charToText
  ]

(runScriptNoFuzz, runScriptWantFuzz) = (runner id, runner not)
    where runner relevant name testCase = 
            let as = name <.> "as"
                wasm = name <.> "wasm"
                fileArg = fromString . encodeString
                script = do Turtle.output as $ fromString testCase
                            res@(exitCode, _, _) <- procStrictWithErr "asc"
                                ["-no-dfinity-api", "-no-check-ir", fileArg as] empty
                            if ExitSuccess == exitCode
                            then (True,) <$> procStrictWithErr "wasm-interp" ["--enable-multi", fileArg wasm] empty
                            else pure (False, res)
            in run script >>= assertSuccessNoFuzz relevant

prop_explodeConcat :: UTF8 String -> Property
prop_explodeConcat (UTF8 str) = monadicIO $ do
  let testCase :: String
      testCase = "{ var str = \"\"; for (c in \""
                 <> s <> "\".chars()) { str #= charToText c }; assert (str == \"" <> s <> "\") }"

      s = concatMap escape str
  runScriptNoFuzz "explodeConcat" testCase

-- TODO: why can't we use Test.QuickCheck.Unicode.Unicode? (see https://github.com/bos/quickcheck-unicode/issues/5)
newtype UTF8 a = UTF8 a deriving Show

instance Arbitrary (UTF8 String) where
  arbitrary = UTF8 <$> string1

instance Arbitrary (UTF8 Char) where
  arbitrary = UTF8 <$> Test.QuickCheck.Unicode.char

hex :: Int -> String
hex = (`showHex` "")

escape ch | '\\' `elem` show ch = "\\u{" <> hex (fromEnum ch) <> "}"
escape '"' = "\\\""
escape ch = pure ch

prop_charToText (UTF8 char) = monadicIO $ do
  let testCase = "assert (switch ((charToText '"
                 <> c <> "').chars().next()) { case (?'" <> c <> "') true; case _ false })"

      c = escape char
  runScriptNoFuzz "charToText" testCase

assertSuccessNoFuzz relevant (compiled, (exitCode, out, err)) = do
  let fuzzErr = not $ Data.Text.null err
  when fuzzErr $ do
    monitor (counterexample "STDERR:")
    monitor (counterexample . Data.Text.unpack $ err)
  let fuzzOut = not $ Data.Text.null out
  let fuzzOutRelevant = relevant fuzzOut
  when (fuzzOut && fuzzOutRelevant) $ do
    monitor (counterexample "STDOUT:")
    monitor (counterexample . Data.Text.unpack $ out)
  assert (not $ ExitSuccess /= exitCode || (if compiled then fuzzOutRelevant else fuzzOut) || fuzzErr)

newtype Failing a = Failing a deriving Show

instance Arbitrary (Failing String) where
  arbitrary = do let failed as = "let _ = " ++ unparseAS as ++ ";"
                 Failing . failed <$> suchThat (resize 5 arbitrary) (\(evaluate @ Integer -> res) -> null res)

prop_rejects (Failing testCase) = monadicIO $ runScriptWantFuzz "fails" testCase

halve [] = ([], [])
halve a@[_] = (a, [])
halve (clown : joker : (halve -> (cs, js))) = (clown : cs, joker : js)

newtype TestCase = TestCase [String] deriving Show

instance Arbitrary TestCase where
  arbitrary = do tests <- infiniteListOf arbitrary
                 let expected = evaluate @ Integer <$> tests
                 let paired as = fmap (\res -> "assert (" ++ unparseAS as ++ " == " ++ show res ++ ");")
                 pure . TestCase . take 100 . catMaybes $ zipWith paired tests expected




prop_verifies (TestCase (map fromString -> testCase)) = monadicIO $ do
  let script cases = do Turtle.output "tests.as" $ msum cases
                        res@(exitCode, _, _) <- procStrictWithErr "asc"
                                 ["-no-dfinity-api", "-no-check-ir", "tests.as"] empty
                        if ExitSuccess == exitCode
                        then (True,) <$> procStrictWithErr "wasm-interp" ["--enable-multi", "tests.wasm"] empty
                        else pure (False, res)
  res@(compiled, (exitCode, out, err)) <- run $ script testCase
  when compiled $ do
    pre (ExitSuccess == exitCode)
    let bisect (clowns, jokers) =
            do (True, (cExitCode, cOut, _)) <- script clowns
               (True, (jExitCode, jOut, _)) <- script jokers
               case (Data.Text.null cOut, Data.Text.null jOut) of
                 (False, _) -> if length clowns == 1
                               then reduce (Fold (<>) empty linesToText) $ sequence clowns
                               else bisect $ halve clowns
                 (_, False) -> bisect $ halve jokers
    let good = Data.Text.null out
    unless good $ (run . bisect $ halve testCase) >>= monitor . (counterexample . Data.Text.unpack $)
  assertSuccessNoFuzz id res


data Off = TwoLess | OneLess | OneMore | TwoMore
 deriving (Enum, Eq, Ord, Show)

instance Arbitrary Off where
  arbitrary = elements [TwoLess .. TwoMore]

-- Below data structure tries to focus test case generation of
-- numeric values to *neuralgic points* of the numeric line, namely
-- areas where (from our white-box knowledge of ActorScript's inner
-- workings) representation changes are expected to happen. These
-- are mostly at a power-of-two boundary and around it, e.g. we have
-- signed 30 bit numbers (`Int` and `Nat`) that have a compact representation
-- and out of this range the number gets heap-allocated.
--
data Neuralgic t
  = LargeNeg
  | AroundNeg Integer
  | Around0
  | AroundPos Integer
  | LargePos
  | Offset (Neuralgic t) Off
 deriving (Eq, Ord, Show)

instance Arbitrary (Neuralgic Integer) where
  arbitrary = frequency [ (5, elements [LargeNeg, AroundNeg 63, AroundNeg 30, Around0, AroundPos 30, AroundPos 63, LargePos])
                        , (8, Offset <$> arbitrary <*> arbitrary)]


pred `guardedFrom` set = frequency [ (5, elements set)
                                   , (3, Offset <$> arbitrary <*> arbitrary)]
                         `suchThat` (isJust . guardedEvalN pred)
infix 1 `guardedFrom`

instance Arbitrary (Neuralgic Natural) where
  arbitrary =  (\n -> if n >= 0 then pure (fromIntegral n) else Nothing)
               `guardedFrom` [Around0, AroundPos 30, AroundPos 63, LargePos]

instance KnownNat n => Arbitrary (Neuralgic (BitLimited n Natural)) where
  arbitrary = fmap NatN <$> trapNat bits `guardedFrom` menu bits
    where bits = natVal (Proxy @n)
          menu 8 = [Around0, AroundPos 3, AroundPos 5, AroundPos 8]
          menu 16 = [Around0, AroundPos 3, AroundPos 5, AroundPos 8, AroundPos 13, AroundPos 16]
          menu 32 = [Around0, AroundPos 8, AroundPos 13, AroundPos 16, AroundPos 23, AroundPos 32]
          menu 64 = [Around0, AroundPos 8, AroundPos 13, AroundPos 23, AroundPos 31, AroundPos 47, AroundPos 64]


instance KnownNat n => Arbitrary (Neuralgic (BitLimited n Integer)) where
  arbitrary = fmap IntN <$> trapInt bits `guardedFrom` menu bits
    where bits = natVal (Proxy @n)
          menu 8 = [Around0, AroundNeg 3, AroundNeg 5, AroundNeg 7, AroundPos 3, AroundPos 5, AroundPos 7]
          menu 16 = [Around0, AroundNeg 3, AroundNeg 7, AroundNeg 10, AroundNeg 15, AroundPos 3, AroundPos 8, AroundPos 10, AroundPos 15]
          menu 32 = [Around0, AroundNeg 3, AroundNeg 17, AroundNeg 27, AroundNeg 31, AroundPos 3, AroundPos 18, AroundPos 25, AroundPos 31]
          menu 64 = [Around0, AroundNeg 9, AroundNeg 27, AroundNeg 51, AroundNeg 63, AroundPos 10, AroundPos 28, AroundPos 55, AroundPos 63]


instance KnownNat n => Arbitrary (Neuralgic (BitLimited n Word)) where
  arbitrary = fmap WordN <$> trapWord bits `guardedFrom` menu bits
    where bits = natVal (Proxy @n)
          menu 8 = [Around0, AroundNeg 3, AroundNeg 5, AroundNeg 8, AroundPos 3, AroundPos 5, AroundPos 8]
          menu 16 = [Around0, AroundNeg 3, AroundNeg 12, AroundNeg 16, AroundPos 6, AroundPos 13, AroundPos 16]
          menu 32 = [Around0, AroundNeg 3, AroundNeg 12, AroundNeg 23, AroundNeg 32, AroundPos 6, AroundPos 15, AroundPos 26, AroundPos 32]
          menu 64 = [Around0, AroundNeg 3, AroundNeg 11, AroundNeg 21, AroundNeg 31, AroundNeg 42, AroundNeg 64, AroundPos 6, AroundPos 14, AroundPos 27, AroundPos 43, AroundPos 57, AroundPos 64]


data ASTerm :: * -> * where
  -- Comparisons
  NotEqual, Equals, GreaterEqual, Greater, LessEqual, Less
    :: (Annot a, Literal a, Evaluatable a) => ASTerm a -> ASTerm a -> ASTerm Bool
  -- Short-circuit
  ShortAnd, ShortOr
    :: ASTerm Bool -> ASTerm Bool -> ASTerm Bool
  -- Boolean
  Not :: ASTerm Bool -> ASTerm Bool
  Bool :: Bool -> ASTerm Bool
  -- Bitwise
  Complement :: ASTerm (BitLimited n Word) -> ASTerm (BitLimited n Word)
  Or, And, Xor, RotL, RotR, ShiftL, ShiftR, ShiftRSigned
    :: ASTerm (BitLimited n Word) -> ASTerm (BitLimited n Word) -> ASTerm (BitLimited n Word)
  PopCnt, Clz, Ctz :: ASTerm (BitLimited n Word) -> ASTerm (BitLimited n Word)
  -- Arithmetic
  Pos, Neg, Abs :: ASTerm a -> ASTerm a
  Add, Sub, Mul, Div, Mod, Pow :: ASTerm a -> ASTerm a -> ASTerm a
  -- Numeric
  Neuralgic :: Neuralgic a -> ASTerm a
  Five :: ASTerm a
  -- Conditional
  IfThenElse :: ASTerm a -> ASTerm a -> ASTerm Bool -> ASTerm a
  -- Conversion
  ConvertNatural :: ASTerm Natural -> ASTerm Integer
  ConvertNat :: KnownNat n => ASTerm (BitLimited n Natural) -> ASTerm Integer
  ConvertInt :: KnownNat n => ASTerm (BitLimited n Integer) -> ASTerm Integer
  ConvertWord :: WordLike n => ASTerm (BitLimited n Word) -> ASTerm Integer
  -- Constructors (intro forms)
  Pair :: (Annot a, Annot b, Literal a, Literal b, Evaluatable a, Evaluatable b) => ASTerm a -> ASTerm b -> ASTerm (a, b)
  Triple :: (Evaluatable a, Evaluatable b, Evaluatable c) => ASTerm a -> ASTerm b -> ASTerm c -> ASTerm (a, b, c)
  Array :: ASTerm a -> ASTerm [a] -- not matchable!
  Null :: ASTerm (Maybe a)
  Some :: Evaluatable a => ASTerm a -> ASTerm (Maybe a)
  -- Variants, Objects (TODO)

deriving instance Show (ASTerm t)

subTerm :: Arbitrary (ASTerm t) => Bool -> Int -> [(Int, Gen (ASTerm t))]
subTerm fullPow n =
    [ (1, resize (n `div` 5) $ Pow <$> arbitrary <*> arbitrary) | fullPow] ++
    [ (n, resize (n `div` 3) $ Add <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Sub <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Mul <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Div <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Mod <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 4) $ IfThenElse <$> arbitrary <*> arbitrary <*> arbitrary)
    ]

subTermPow :: Arbitrary (ASTerm t) => (ASTerm t -> ASTerm t) -> Int -> [(Int, Gen (ASTerm t))]
subTermPow mod n = (n, resize (n `div` 5) $ Pow <$> arbitrary <*> (mod <$> arbitrary))
                   : subTerm False n

subTermPow5 :: Arbitrary (ASTerm t)
               => Int -> [(Int, Gen (ASTerm t))]
subTermPow5 n = (n, resize (n `div` 5)
                      $ Pow <$> arbitrary
                            <*> (Neuralgic <$> elements [ Around0
                                                    , Around0 `Offset` OneMore
                                                    , AroundPos 1
                                                    , AroundPos 1 `Offset` OneMore
                                                    , AroundPos 2
                                                    ]))
                : subTerm False n

bitwiseTerm :: WordLike n => Arbitrary (ASTerm (BitLimited n Word)) => Int -> [(Int, Gen (ASTerm (BitLimited n Word)))]
bitwiseTerm n =
    [ (n `div` 5, resize (n `div` 3) $ Or <$> arbitrary <*> arbitrary)
    , (n `div` 5, resize (n `div` 3) $ And <$> arbitrary <*> arbitrary)
    , (n `div` 5, resize (n `div` 3) $ Xor <$> arbitrary <*> arbitrary)
    , (n `div` 5, resize (n `div` 3) $ RotL <$> arbitrary <*> arbitrary)
    , (n `div` 5, resize (n `div` 3) $ RotR <$> arbitrary <*> arbitrary)
    , (n `div` 5, resize (n `div` 3) $ ShiftL <$> arbitrary <*> arbitrary)
    , (n `div` 5, resize (n `div` 3) $ ShiftR <$> arbitrary <*> arbitrary)
    , (n `div` 5, resize (n `div` 3) $ ShiftRSigned <$> arbitrary <*> arbitrary)
    , (n `div` 5, PopCnt <$> arbitrary)
    , (n `div` 5, Clz <$> arbitrary)
    , (n `div` 5, Ctz <$> arbitrary)
    , (n `div` 5, Complement <$> arbitrary)
    ]

-- generate reasonably formed trees from smaller subterms
--
reasonablyShaped :: (Arbitrary (Neuralgic a), Annot a, Evaluatable a, Literal a)
                 => (Int -> [(Int, Gen (ASTerm a))])
                 -> Gen (ASTerm a)
reasonablyShaped sub = sized $ \(succ -> n) -> frequency $
                       (30 `div` n, Neuralgic <$> arbitrary)
                       : if n > 1 then sub n else []

instance {-# OVERLAPPABLE #-} KnownNat n => Arbitrary (ASTerm (BitLimited n Natural)) where
  arbitrary = reasonablyShaped $ subTermPow (`Mod` Five)

instance {-# OVERLAPS #-} Arbitrary (ASTerm Nat8) where
  arbitrary = reasonablyShaped $ subTerm True


instance {-# OVERLAPPABLE #-} KnownNat n => Arbitrary (ASTerm (BitLimited n Integer)) where
  arbitrary = reasonablyShaped subTermPow5

instance {-# OVERLAPS #-} Arbitrary (ASTerm Int8) where
  arbitrary = reasonablyShaped $ subTerm True


instance {-# OVERLAPPABLE #-} WordLike n => Arbitrary (ASTerm (BitLimited n Word)) where
  arbitrary = reasonablyShaped $ (<>) <$> subTermPow (`Mod` Five) <*> bitwiseTerm

instance {-# OVERLAPS #-} Arbitrary (ASTerm Word8) where
  arbitrary = reasonablyShaped $ (<>) <$> subTerm True <*> bitwiseTerm

instance (Annot a, Annot b, Literal a, Literal b, Evaluatable a, Evaluatable b, Arbitrary (ASTerm a), Arbitrary (ASTerm b)) => Arbitrary (ASTerm (a, b)) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Arbitrary (ASTerm Bool) where
  arbitrary = sized $ \(succ -> n) -> -- TODO: use frequency?
    oneof $ (Bool <$> arbitrary) : if n <= 1 then [] else
    [ resize (n `div` 3) $ elements [NotEqual @Integer, Equals, GreaterEqual, Greater, LessEqual, Less] <*> arbitrary <*> arbitrary
    , resize (n `div` 5) $ elements [ShortAnd, ShortOr] <*> arbitrary <*> arbitrary
    , resize (n `div` 2) $ Not <$> arbitrary
    ]

instance Arbitrary (ASTerm Natural) where
  arbitrary = reasonablyShaped $ subTermPow (`Mod` Five)

instance Arbitrary (ASTerm Integer) where
  arbitrary = reasonablyShaped $ \n ->
    [ (n, resize (n `div` 2) $ Pos <$> arbitrary)
    , (n, resize (n `div` 2) $ Neg <$> arbitrary)
    , (n, resize (n `div` 2) $ Abs <$> arbitrary)
    , (n, ConvertNatural <$> arbitrary)
    , (n `div` 2, ConvertNat <$> (arbitrary @(ASTerm Nat8)))
    , (n `div` 2, ConvertNat <$> (arbitrary @(ASTerm Nat16)))
    , (n `div` 2, ConvertNat <$> (arbitrary @(ASTerm Nat32)))
    , (n `div` 2, ConvertNat <$> (arbitrary @(ASTerm Nat64)))
    , (n `div` 3, ConvertWord <$> (arbitrary @(ASTerm Word8)))
    , (n `div` 3, ConvertWord <$> (arbitrary @(ASTerm Word16)))
    , (n `div` 3, ConvertWord <$> (arbitrary @(ASTerm Word32)))
    , (n `div` 3, ConvertWord <$> (arbitrary @(ASTerm Word64)))
    , (n `div` 3, ConvertInt <$> (arbitrary @(ASTerm Int8)))
    , (n `div` 3, ConvertInt <$> (arbitrary @(ASTerm Int16)))
    , (n `div` 3, ConvertInt <$> (arbitrary @(ASTerm Int32)))
    , (n `div` 3, ConvertInt <$> (arbitrary @(ASTerm Int64)))
    ] <> subTermPow ((`Mod` Five) . Abs) n

instance Num a => Num (Maybe a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = liftA abs
  signum = liftA signum
  fromInteger = pure . fromInteger

instance Real a => Real (Maybe a) where
  toRational = undefined

instance Enum a => Enum (Maybe a)

instance (Enum (Maybe a), Eq a, Integral a) => Integral (Maybe a) where
  _ `quotRem` Just 0 = (Nothing, Nothing)
  a `quotRem` b = case liftA2 quotRem a b of
                    Just (pure -> x, pure -> y) -> (x, y)
                    _ -> (Nothing, Nothing)


evalO TwoLess = \n -> n - 2
evalO OneLess = \n -> n - 1
evalO OneMore = (+1)
evalO TwoMore = (+2)


evalN LargeNeg = fromIntegral (minBound :: Int) - 111
evalN (AroundNeg n) = - 2 ^ n
evalN Around0 = 0
evalN (AroundPos n) = 2 ^ n - 1
evalN LargePos = fromIntegral (maxBound :: Int) + 333
evalN (Offset (evalN -> n) (evalO -> o)) = o n


guardedEvalN :: Integral a => (Integer -> Maybe a) -> Neuralgic a -> Maybe a
guardedEvalN g (Offset (guardedEvalN g -> n) (evalO -> o)) = g =<< (o . toInteger) <$> n
guardedEvalN g (g . evalN -> res) = res

noExponentRestriction, nonNegativeExponentRestriction, defaultExponentRestriction :: (Ord a, Num a) => a -> Maybe ()
noExponentRestriction _ = pure ()
nonNegativeExponentRestriction = guard . (>= 0)
defaultExponentRestriction = guard . ((&&) <$> (>= 0) <*> (< 5))

class Restricted a where
  substractable :: Maybe a -> Maybe a -> Bool
  exponentiable :: (Num a, Ord a) => a -> Maybe ()
  exponentiable = defaultExponentRestriction

instance Restricted Integer where substractable _ _ = True
instance Restricted Natural where substractable a b = isJust $ do m <- a; n <- b; guard $ m >= n
instance KnownNat n => Restricted (BitLimited n Natural) where
  substractable a b = isJust $ do NatN m <- a; NatN n <- b; guard $ m >= n
  exponentiable = if natVal (Proxy @n) <= 8
                  then noExponentRestriction
                  else defaultExponentRestriction
instance KnownNat n => Restricted (BitLimited n Integer) where
  exponentiable = if natVal (Proxy @n) <= 8
                  then nonNegativeExponentRestriction
                  else defaultExponentRestriction
instance KnownNat n => Restricted (BitLimited n Word) where
  exponentiable = if natVal (Proxy @n) <= 8
                  then noExponentRestriction
                  else defaultExponentRestriction

class Ord a => Evaluatable a where
  evaluate :: ASTerm a -> Maybe a


data BitLimited (n :: Nat) (a :: *) where
  NatN :: KnownNat n => Natural -> BitLimited n Natural
  IntN :: KnownNat n => Integer -> BitLimited n Integer
  WordN :: KnownNat n => Natural -> BitLimited n Word

deriving instance Show (BitLimited n a)
deriving instance Eq (BitLimited n a)
deriving instance Ord (BitLimited n a)


-- for a BitLimited n Word, the instances of WordView provide the
-- change of perspective to the built-in Word types' semantics
--
class WordView a where
  type WordType a :: *
  toWord :: a -> WordType a
  fromWord :: WordType a -> a
  lift1 :: (WordType a -> WordType a) -> a -> a
  lift1 f (fromWord . f . toWord -> res) = res
  lift1b :: (WordType a -> b) -> a -> b
  lift1b f (f . toWord -> res) = res
  lift2 :: (WordType a -> WordType a -> WordType a) -> a -> a -> a
  lift2 f (toWord -> a) (toWord -> b) = fromWord (a `f` b)
  {-# MINIMAL toWord, fromWord #-}


type family WordTypeForBits a where
  WordTypeForBits 8 = Data.Word.Word8
  WordTypeForBits 16 = Data.Word.Word16
  WordTypeForBits 32 = Data.Word.Word32
  WordTypeForBits 64 = Data.Word.Word64


instance (Integral (WordTypeForBits n), FiniteBits (WordTypeForBits n), KnownNat n) => WordView (BitLimited n Word) where
  type WordType (BitLimited n Word) = WordTypeForBits n
  toWord (WordN n) = fromIntegral n
  fromWord = WordN . fromIntegral

type WordLike n = ( Integral (WordTypeForBits n)
                  , FiniteBits (WordTypeForBits n)
                  , KnownNat n
                  , WordView (BitLimited n Word))

instance WordLike n => Bits (BitLimited n Word) where
  (.&.) = lift2 (.&.)
  (.|.) = lift2 (.|.)
  xor = lift2 xor
  complement = lift1 complement
  shift a b = lift1 (`shift` b) a
  rotate a b = lift1 (`rotate` b) a
  bitSize = finiteBitSize
  bitSizeMaybe = pure . finiteBitSize
  isSigned _ = False
  testBit a b = lift1b (`testBit` b) a
  bit = fromWord . bit
  popCount = lift1b popCount

instance WordLike n => FiniteBits (BitLimited n Word) where
  finiteBitSize b = fromIntegral $ natVal (Proxy @n)
  countLeadingZeros = lift1b countLeadingZeros
  countTrailingZeros = lift1b countTrailingZeros

type Nat8 = BitLimited 8 Natural
type Nat16 = BitLimited 16 Natural
type Nat32 = BitLimited 32 Natural
type Nat64 = BitLimited 64 Natural
type Int8 = BitLimited 8 Integer
type Int16 = BitLimited 16 Integer
type Int32 = BitLimited 32 Integer
type Int64 = BitLimited 64 Integer
type Word8 = BitLimited 8 Word
type Word16 = BitLimited 16 Word
type Word32 = BitLimited 32 Word
type Word64 = BitLimited 64 Word

instance ToBitLimited bits a => Num (BitLimited bits a) where
  NatN m + NatN n = NatN . fromJust $ trapNat (natVal (Proxy @bits)) (toInteger m + toInteger n)
  IntN m + IntN n = IntN . fromJust $ trapInt (natVal (Proxy @bits)) (m + n)
  WordN m + WordN n = WordN . fromJust $ trapWord (natVal (Proxy @bits)) (toInteger m + toInteger n)
  NatN m - NatN n = NatN . fromJust $ trapNat (natVal (Proxy @bits)) (toInteger m - toInteger n)
  IntN m - IntN n = IntN . fromJust $ trapInt (natVal (Proxy @bits)) (m - n)
  WordN m - WordN n = WordN . fromJust $ trapWord (natVal (Proxy @bits)) (toInteger m - toInteger n)
  NatN m * NatN n = NatN . fromJust $ trapNat (natVal (Proxy @bits)) (toInteger m * toInteger n)
  IntN m * IntN n = IntN . fromJust $ trapInt (natVal (Proxy @bits)) (m * n)
  WordN m * WordN n = WordN . fromJust $ trapWord (natVal (Proxy @bits)) (toInteger m * toInteger n)

  abs (NatN m) = NatN (abs m)
  abs (IntN m) = IntN . fromJust $ trapInt (natVal (Proxy @bits)) (abs m)
  abs (WordN m) = WordN (abs m)

  signum (NatN m) = NatN (signum m)
  signum (IntN m) = IntN (signum m)
  signum (WordN m) = WordN (signum m)

  fromInteger = toBitLimited . fromInteger

-- Helper class ToBitLimited, to build correct BitLimited variant
class KnownNat bits => ToBitLimited bits a where toBitLimited :: Integer -> BitLimited bits a

instance KnownNat bits => ToBitLimited bits Natural where
  toBitLimited = NatN @bits . fromJust . trapNat (natVal (Proxy @bits)) . fromIntegral
instance KnownNat bits => ToBitLimited bits Integer where
  toBitLimited = IntN @bits . fromJust . trapInt (natVal (Proxy @bits))
instance KnownNat bits => ToBitLimited bits Word where
  toBitLimited = WordN @bits . fromJust . trapWord (natVal (Proxy @bits)) . fromIntegral

instance ToBitLimited n a => Real (BitLimited n a)
instance Enum (BitLimited n a)
instance ToBitLimited n a => Integral (BitLimited n a) where
  toInteger (NatN n) = toInteger n
  toInteger (IntN n) = n
  toInteger (WordN n) = toInteger n

trapNat :: Integer -> Integer -> Maybe Natural
trapNat n v = do guard (v >= 0 && v < 2 ^ n); pure (fromIntegral v)

trapInt :: Integer -> Integer -> Maybe Integer
trapInt (pred -> n) v = do guard (v < 2 ^ n && v >= - 2 ^ n); pure v

trapWord :: Integer -> Integer -> Maybe Natural
trapWord n v = pure . fromIntegral $ v `mod` 2 ^ n


instance KnownNat bits => Evaluatable (BitLimited bits Natural) where
  evaluate Five = pure $ NatN 5
  evaluate (Neuralgic n) = NatN <$> trapNat (natVal (Proxy @bits)) (evalN n)
  evaluate ab =
      case ab of
        a `Add` b -> go (+) a b
        a `Sub` b -> go (-) a b
        a `Mul` b -> go (*) a b
        _ `Div` (evaluate -> Just 0) -> Nothing
        a `Div` b -> go quot a b
        _ `Mod` (evaluate -> Just 0) -> Nothing
        a `Mod` b -> go rem a b
        a `Pow` b -> do b' <- evaluate b; exponentiable b'; go (^) a b
        IfThenElse a b c -> do c <- evaluate c
                               evaluate $ if c then a else b
        _ -> error $ show ab
    where go op a b = do NatN a <- evaluate a; NatN b <- evaluate b; NatN <$> trapNat (natVal (Proxy @bits)) (toInteger a `op` toInteger b)

instance KnownNat bits => Evaluatable (BitLimited bits Integer) where
  evaluate Five = pure $ IntN 5
  evaluate (Neuralgic n) = IntN <$> trapInt (natVal (Proxy @bits)) (evalN n)
  evaluate ab =
      case ab of
        a `Add` b -> go (+) a b
        a `Sub` b -> go (-) a b
        a `Mul` b -> go (*) a b
        _ `Div` (evaluate -> Just 0) -> Nothing
        a `Div` b -> go quot a b
        _ `Mod` (evaluate -> Just 0) -> Nothing
        a `Mod` b -> go rem a b
        a `Pow` b -> do b' <- evaluate b; exponentiable b'; go (^) a b
        IfThenElse a b c -> do c <- evaluate c
                               evaluate $ if c then a else b
        _ -> error $ show ab
    where go op a b = do IntN a <- evaluate a; IntN b <- evaluate b; IntN <$> trapInt (natVal (Proxy @bits)) (toInteger a `op` toInteger b)


instance WordLike bits => Evaluatable (BitLimited bits Word) where
  evaluate Five = pure $ WordN 5
  evaluate (Neuralgic n) = WordN <$> trapWord (natVal (Proxy @bits)) (evalN n)
  evaluate (Complement a) = complement <$> evaluate a
  evaluate ab =
      case ab of
        a `Add` b -> go (+) a b
        a `Sub` b -> go (-) a b
        a `Mul` b -> go (*) a b
        _ `Div` (evaluate -> Just 0) -> Nothing
        a `Div` b -> go quot a b
        _ `Mod` (evaluate -> Just 0) -> Nothing
        a `Mod` b -> go rem a b
        a `Pow` b -> do b' <- evaluate b; exponentiable b'; go (^) a b
        IfThenElse a b c -> do c <- evaluate c
                               evaluate $ if c then a else b
        a `Or` b -> log (.|.) a b
        a `And` b -> log (.&.) a b
        a `Xor` b -> log xor a b
        a `RotL` b -> log (flip rotateL . fromIntegral) b a
        a `RotR` b -> log (flip rotateR . fromIntegral) b a
        a `ShiftL` b -> log (flip shiftL . (`mod` fromIntegral bitcount) . fromIntegral) b a
        a `ShiftR` b -> log (flip shiftR . (`mod` fromIntegral bitcount) . fromIntegral) b a
        a `ShiftRSigned` b -> log signedShiftR a b
        PopCnt (evaluate -> a) -> fromIntegral . popCount <$> a
        Clz (evaluate -> a) -> fromIntegral . countLeadingZeros <$> a
        Ctz (evaluate -> a) -> fromIntegral . countTrailingZeros <$> a
    where log op a b = op <$> evaluate a <*> evaluate b
          bitcount = natVal (Proxy @bits)
          signedShiftR a b = fromIntegral $ a' `shiftR` (fromIntegral b `mod` fromIntegral bitcount)
            where a' = toInteger a - (toInteger (((a `rotateL` 1) .&. 1) `rotateR` 1) `shiftL` 1)
          go op a b = do WordN a <- evaluate a; WordN b <- evaluate b; WordN <$> trapWord bitcount (toInteger a `op` toInteger b)

instance Evaluatable Integer where
  evaluate = eval
instance Evaluatable Natural where
  evaluate = eval

instance (Evaluatable a, Evaluatable b) => Evaluatable (a, b) where
  evaluate (Pair a b) = (,) <$> evaluate a <*> evaluate b

eval :: (Restricted a, Integral a) => ASTerm a -> Maybe a
eval Five = pure 5
eval (Neuralgic n) = evalN n
eval (Pos n) = eval n
eval (Neg n) = - eval n
eval (Abs n) = abs $ eval n
eval (a `Add` b) = eval a + eval b
eval ((eval -> a) `Sub` (eval -> b)) = do guard $ substractable a b; a - b
eval (a `Mul` b) = eval a * eval b
eval (a `Div` b) = eval a `quot` eval b
eval (a `Mod` b) = eval a `rem` eval b
eval (a `Pow` (eval -> b)) = do b' <- b; exponentiable b'; (^) <$> eval a <*> b
eval (ConvertNatural t) = fromIntegral <$> evaluate t
eval (ConvertNat t) = fromIntegral <$> evaluate t
eval (ConvertInt t) = fromIntegral <$> evaluate t
eval (ConvertWord t) = fromIntegral <$> evaluate t
eval (IfThenElse a b c) = do c <- evaluate c
                             eval $ if c then a else b

--eval (Pair a b) = (,) <$> evaluate a <*> evaluate b
--eval (Triple a b c) = (,,) <$> evaluate a <*> evaluate b <*> evaluate c
--eval Null = Nothing
--eval (Some a) = fmap Just $ evaluate a
--eval _ = Nothing


instance Evaluatable Bool where
  evaluate (a `NotEqual` b) = (/=) <$> evaluate a <*> evaluate b
  evaluate (a `Equals` b) = (==) <$> evaluate a <*> evaluate b
  evaluate (a `GreaterEqual` b) = (>=) <$> evaluate a <*> evaluate b
  evaluate (a `Greater` b) = (>) <$> evaluate a <*> evaluate b
  evaluate (a `LessEqual` b) = (<=) <$> evaluate a <*> evaluate b
  evaluate (a `Less` b) = (<) <$> evaluate a <*> evaluate b
  evaluate (a `ShortAnd` b) = evaluate a >>= bool (pure False) (evaluate b)
  evaluate (a `ShortOr` b) = evaluate a >>= bool (evaluate b) (pure True)
  evaluate (Not a) = not <$> evaluate a
  evaluate (Bool b) = pure b


class Annot t where
  annot :: ASTerm t -> String -> String
  sizeSuffix :: ASTerm t -> String -> String
  sizeSuffix _ = id

instance Annot (a, b) where
  annot _ = id

instance Annot Integer where
  annot _ s = "((" <> s <> ") : Int)"

instance Annot Natural where
  annot _ s = "((" <> s <> ") : Nat)"


bitWidth :: KnownNat n => Proxy n -> String
bitWidth p = show (natVal p)

instance KnownNat n => Annot (BitLimited n Natural) where
  annot _ s = "((" <> s <> ") : Nat" <> bitWidth (Proxy @n) <> ")"

instance KnownNat n => Annot (BitLimited n Integer) where
  annot _ s = "((" <> s <> ") : Int" <> bitWidth (Proxy @n) <> ")"

instance KnownNat n => Annot (BitLimited n Word) where
  annot _ s = "((" <> s <> ") : Word" <> bitWidth (Proxy @n) <> ")"
  sizeSuffix _ = (<> bitWidth (Proxy @n))

instance Annot Bool where
  annot _ = id

class Literal a where
  literal :: Neuralgic a -> String
  literal (evalN -> n) = if n < 0 then "(" <> show n <> ")" else show n

instance Literal (a, b) where
  literal _ = error "Literal (a, b) makes no sense"

instance Literal Integer
instance Literal Natural
instance Literal (BitLimited n Natural)
instance KnownNat bits => Literal (BitLimited bits Integer) where
  -- compensate for https://github.com/dfinity-lab/actorscript/issues/505
  literal (evalN -> n) = if buggy n then "intToInt" <> bitWidth pr <> "(" <> show n <> ")"
                         else if n < 0
                         then "(" <> show n <> ")"
                         else show n
    where buggy n = n == - 2 ^ (numbits - 1)
          numbits = natVal pr
          pr = Proxy @bits
instance KnownNat bits => Literal (BitLimited bits Word) where
  literal n = show . fromJust $ trapWord (natVal (Proxy @bits)) (evalN n)

instance Literal Bool where
  literal _ = error "Literal Bool makes no sense"

inParens :: (a -> String) -> String -> a -> a -> String
inParens to op lhs rhs = "(" <> to lhs <> " " <> op <> " " <> to rhs <> ")"

unparseAS :: (Annot a, Literal a) => ASTerm a -> String
unparseAS f@Five = annot f "5"
unparseAS a@(Neuralgic n) = annot a $ literal n
unparseAS (Pos n) = "(+" <> unparseAS n <> ")"
unparseAS (Neg n) = "(-" <> unparseAS n <> ")"
unparseAS (Abs n) = "(abs " <> unparseAS n <> ")"
unparseAS (a `Add` b) = inParens unparseAS "+" a b
unparseAS (a `Sub` b) = annot a $ inParens unparseAS "-" a b
unparseAS (a `Mul` b) = inParens unparseAS "*" a b
unparseAS (a `Div` b) = inParens unparseAS "/" a b
unparseAS (a `Mod` b) = inParens unparseAS "%" a b
unparseAS (a `Pow` b) = inParens unparseAS "**" a b
unparseAS (a `Or` b) = inParens unparseAS "|" a b
unparseAS (a `And` b) = inParens unparseAS "&" a b
unparseAS (a `Xor` b) = inParens unparseAS "^" a b
unparseAS (a `RotL` b) = inParens unparseAS "<<>" a b
unparseAS (a `RotR` b) = inParens unparseAS "<>>" a b
unparseAS (a `ShiftL` b) = inParens unparseAS "<<" a b
unparseAS (a `ShiftR` b) = inParens unparseAS ">>" a b
unparseAS (a `ShiftRSigned` b) = inParens unparseAS "+>>" a b
unparseAS (PopCnt n) = sizeSuffix n "(popcntWord" <> " " <> unparseAS n <> ")"
unparseAS (Clz n) = sizeSuffix n "(clzWord" <> " " <> unparseAS n <> ")"
unparseAS (Ctz n) = sizeSuffix n "(ctzWord" <> " " <> unparseAS n <> ")"
unparseAS (Complement a) = "(^ " <> unparseAS a <> ")"
unparseAS (ConvertNatural a) = "(++++(" <> unparseAS a <> "))"
unparseAS (ConvertNat a) = unparseNat Proxy a
unparseAS (ConvertInt a) = unparseInt Proxy a
unparseAS (ConvertWord a) = unparseWord Proxy a
unparseAS (IfThenElse a b c) = "(if (" <> unparseAS c <> ") " <> unparseAS a <> " else " <> unparseAS b <> ")"
unparseAS (a `NotEqual` b) = inParens unparseAS "!=" a b
unparseAS (a `Equals` b) = inParens unparseAS "==" a b
unparseAS (a `GreaterEqual` b) = inParens unparseAS ">=" a b
unparseAS (a `Greater` b) = inParens unparseAS ">" a b
unparseAS (a `LessEqual` b) = inParens unparseAS "<=" a b
unparseAS (a `Less` b) = inParens unparseAS "<" a b
unparseAS (a `ShortAnd` b) = inParens unparseAS "and" a b
unparseAS (a `ShortOr` b) = inParens unparseAS "or" a b
unparseAS (Not a) = "(not " <> unparseAS a <> ")"
unparseAS (Bool False) = "false"
unparseAS (Bool True) = "true"
unparseAS (a `Pair` b) = "(" <> unparseAS a <> ", " <> unparseAS b <> ")"

unparseNat :: KnownNat n => Proxy n -> ASTerm (BitLimited n Natural) -> String
unparseNat p a = "(nat" <> bitWidth p <> "ToNat(" <> unparseAS a <> "))"

unparseInt :: KnownNat n => Proxy n -> ASTerm (BitLimited n Integer) -> String
unparseInt p a = "(int" <> bitWidth p <> "ToInt(" <> unparseAS a <> "))"

unparseWord :: KnownNat n => Proxy n -> ASTerm (BitLimited n Word) -> String
unparseWord p a = "(word" <> bitWidth p <> "ToNat(" <> unparseAS a <> "))" -- TODO we want signed too: wordToInt

-- TODOs:
--   - wordToInt
--   - bitwise ops (btst?)
--   - pattern matches (over numeric, bool, structured)
--   - trapping flavour-preserving conversions Nat -> NatN
--   - bitsize-preserving conversions
--   - "abü".len();

data Matching where
  MatchingBool :: (ASTerm Bool, Bool) -> Matching
  MatchingInt :: (ASTerm Integer, Integer) -> Matching
  MatchingPair :: (ASValue a, ASValue b, Show (a, b)) => (ASTerm (a, b), (a, b)) -> Matching

deriving instance Show Matching


instance Arbitrary Matching where
  arbitrary = oneof [ realise MatchingBool <$> gen
                    , realise MatchingPair <$> gen @(Bool, Bool)
                    , realise MatchingPair <$> gen @(Bool, Integer)
                    ]
    where gen :: (Arbitrary (ASTerm a), Evaluatable a) => Gen (ASTerm a, Maybe a)
          gen = (do term <- arbitrary
                    let val = evaluate term
                    pure (term, val)) `suchThat` (isJust . snd)
          realise f (tm, Just v) = f (tm, v)

prop_matchStructured :: Matching -> Property
prop_matchStructured m@(MatchingBool (tm, v)) = monadicIO $ do
  let testCase = "assert (switch (" <> expr <> ") { case (" <> eval'd <> ") true; case _ false })"

      eval'd = unparseValue m
      expr = unparseAS tm
  runScriptNoFuzz "matchStructured" testCase

prop_matchStructured m@(MatchingPair (tm, v)) = monadicIO $ do
  let testCase = "assert (switch (" <> expr <> ") { case (" <> eval'd <> ") true; case _ false })"

      eval'd = unparseValue m
      expr = unparseAS tm
  runScriptNoFuzz "matchStructured" testCase

class ASValue a where
  unparse :: a -> String

instance ASValue Bool where
  unparse = unparseAS . Bool

instance ASValue Integer where
  unparse = show

unparseValue :: Matching -> String
unparseValue (MatchingBool (_, b)) = unparse b
unparseValue (MatchingInt (_, i)) = unparse i
unparseValue (MatchingPair (_, (a, b))) = "(" <> unparse a <> ", " <> unparse b <> ")"
