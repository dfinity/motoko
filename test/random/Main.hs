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
import qualified Data.Text (null, unpack)
import Data.Maybe
import Data.Bool (bool)
import Data.Proxy
import GHC.Natural
import GHC.TypeLits
import qualified Data.Word
import Data.Bits (Bits(..), FiniteBits(..))

import System.Process hiding (proc)
import Turtle
import Debug.Trace (traceShowId)

main = defaultMain tests
  where tests :: TestTree
        tests = testGroup "Arithmetic-logic operations" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "expected failures" $ prop_rejects
  , QC.testProperty "expected successes" $ prop_verifies
  ]



assertSuccessNoFuzz relevant (compiled, (exitCode, out, err)) = do
  let fuzzErr = not $ Data.Text.null err
  when fuzzErr (monitor (counterexample "STDERR:") >> monitor (counterexample . Data.Text.unpack $ err))
  let fuzzOut = not $ Data.Text.null out
  let fuzzOutRelevant = relevant fuzzOut
  when (fuzzOut && fuzzOutRelevant)
           (monitor (counterexample "STDOUT:") >> monitor (counterexample . Data.Text.unpack $ out))
  assert (not $ ExitSuccess /= exitCode || (if compiled then fuzzOutRelevant else fuzzOut) || fuzzErr)

newtype Failing a = Failing a deriving Show

instance Arbitrary (Failing String) where
  arbitrary = do let failed as = "let _ = " ++ showAS as ++ ";"
                 Failing . failed <$> suchThat (resize 5 arbitrary) (\(evaluate @ Integer -> res) -> null res)

prop_rejects (Failing testCase) = monadicIO $ do
  let script = do Turtle.output "fails.as" $ fromString testCase
                  res@(exitCode, _, _) <- procStrictWithErr "asc"
                           ["-no-dfinity-api", "-no-check-ir", "fails.as"] empty
                  if ExitSuccess == exitCode
                  then (True,) <$> procStrictWithErr "wasm-interp" ["--enable-multi", "fails.wasm"] empty
                  else pure (False, res)
  run script >>= assertSuccessNoFuzz not


halve [] = ([], [])
halve a@[_] = (a, [])
halve (clown : joker : (halve -> (cs, js))) = (clown : cs, joker : js)

newtype TestCase = TestCase [String] deriving Show

instance Arbitrary TestCase where
  arbitrary = do tests <- infiniteListOf arbitrary
                 let expected = evaluate @ Integer <$> tests
                 let paired as = fmap (\res -> "assert (" ++ showAS as ++ " == " ++ show res ++ ");")
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
                               then do it <- reduce (Fold (<>) empty linesToText) $ sequence clowns
                                       pure it
                               else bisect $ halve clowns
                 (_, False) -> bisect $ halve jokers
    let good = Data.Text.null out
    unless good $ (run . bisect $ halve testCase) >>= monitor . (counterexample . Data.Text.unpack $)
  assertSuccessNoFuzz id res


data Off = TwoLess | OneLess | OneMore | TwoMore
 deriving (Enum, Eq, Ord, Show)

instance Arbitrary Off where
  arbitrary = elements [TwoLess .. TwoMore]

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

instance Arbitrary (Neuralgic Nat8) where
  arbitrary = fmap NatN <$> trapNat 8 `guardedFrom` [Around0, AroundPos 3, AroundPos 5, AroundPos 8]

instance Arbitrary (Neuralgic Nat16) where
  arbitrary = fmap NatN <$> trapNat 16 `guardedFrom` [Around0, AroundPos 3, AroundPos 5, AroundPos 8, AroundPos 13, AroundPos 16]

instance Arbitrary (Neuralgic Nat32) where
  arbitrary = fmap NatN <$> trapNat 32 `guardedFrom` [Around0, AroundPos 8, AroundPos 13, AroundPos 16, AroundPos 23, AroundPos 32]

instance Arbitrary (Neuralgic Nat64) where
  arbitrary = fmap NatN <$> trapNat 64 `guardedFrom` [Around0, AroundPos 8, AroundPos 13, AroundPos 23, AroundPos 31, AroundPos 47, AroundPos 64]



instance Arbitrary (Neuralgic Int8) where
  arbitrary = fmap IntN <$> trapInt 8 `guardedFrom` [Around0, AroundNeg 3, AroundNeg 5, AroundNeg 7, AroundPos 3, AroundPos 5, AroundPos 7]

instance Arbitrary (Neuralgic Int16) where
  arbitrary = fmap IntN <$> trapInt 16 `guardedFrom` [Around0, AroundNeg 3, AroundNeg 7, AroundNeg 10, AroundNeg 15, AroundPos 3, AroundPos 8, AroundPos 10, AroundPos 15]

instance Arbitrary (Neuralgic Int32) where
  arbitrary = fmap IntN <$> trapInt 32 `guardedFrom` [Around0, AroundNeg 3, AroundNeg 17, AroundNeg 27, AroundNeg 31, AroundPos 3, AroundPos 18, AroundPos 25, AroundPos 31]

instance Arbitrary (Neuralgic Int64) where
  arbitrary = fmap IntN <$> trapInt 64 `guardedFrom` [Around0, AroundNeg 9, AroundNeg 27, AroundNeg 51, AroundNeg 63, AroundPos 10, AroundPos 28, AroundPos 55, AroundPos 63]




instance Arbitrary (Neuralgic Word8) where
  arbitrary = frequency [ (5, elements [Around0, AroundNeg 3, AroundNeg 5, AroundNeg 8, AroundPos 3, AroundPos 5, AroundPos 8])
                        , (3, Offset <$> arbitrary <*> arbitrary)]
        --      `suchThat` (isJust . guardedEvalN ((fmap WordN <$> trapWord 8) . traceShowId .error "SHIT"))  -- why is this never filtered?

instance Arbitrary (Neuralgic Word16) where
  arbitrary = frequency [ (5, elements [Around0, AroundNeg 3, AroundNeg 12, AroundNeg 16, AroundPos 6, AroundPos 13, AroundPos 16])
                        , (3, Offset <$> arbitrary <*> arbitrary)]

instance Arbitrary (Neuralgic Word32) where
  arbitrary = frequency [ (5, elements [Around0, AroundNeg 3, AroundNeg 12, AroundNeg 23, AroundNeg 32, AroundPos 6, AroundPos 15, AroundPos 26, AroundPos 32])
                        , (3, Offset <$> arbitrary <*> arbitrary)]

instance Arbitrary (Neuralgic Word64) where
  arbitrary = frequency [ (5, elements [Around0, AroundNeg 3, AroundNeg 11, AroundNeg 21, AroundNeg 31, AroundNeg 42, AroundNeg 64, AroundPos 6, AroundPos 14, AroundPos 27, AroundPos 43, AroundPos 57, AroundPos 64])
                        , (3, Offset <$> arbitrary <*> arbitrary)]

data ActorScriptTerm a
  = About a
  | Pos (ActorScriptTerm a)
  | Neg (ActorScriptTerm a)
  | Abs (ActorScriptTerm a)
  | ActorScriptTerm a `Add` ActorScriptTerm a
  | ActorScriptTerm a `Sub` ActorScriptTerm a
  | ActorScriptTerm a `Mul` ActorScriptTerm a
  | ActorScriptTerm a `Div` ActorScriptTerm a
  | ActorScriptTerm a `Mod` ActorScriptTerm a
  | ActorScriptTerm a `Pow` ActorScriptTerm a
  | ActorScriptTerm a `Or` ActorScriptTerm a
  | ActorScriptTerm a `And` ActorScriptTerm a
  | ActorScriptTerm a `Xor` ActorScriptTerm a
  | ActorScriptTerm a `RotL` ActorScriptTerm a
  | ActorScriptTerm a `RotR` ActorScriptTerm a
  | ActorScriptTerm a `ShiftL` ActorScriptTerm a
  | ActorScriptTerm a `ShiftR` ActorScriptTerm a
  | ActorScriptTerm a `ShiftRSigned` ActorScriptTerm a
  | PopCnt (ActorScriptTerm a)
  | Clz (ActorScriptTerm a)
  | Ctz (ActorScriptTerm a)
  | Five
  | ConvertNatural (ActorScriptTerm (Neuralgic Natural))
  | forall n . KnownNat n => ConvertNat (ActorScriptTerm (Neuralgic (BitLimited n Natural)))
  | forall n . KnownNat n => ConvertInt (ActorScriptTerm (Neuralgic (BitLimited n Integer)))
  | forall n . WordLike n => ConvertWord (ActorScriptTerm (Neuralgic (BitLimited n Word)))
  -- | ActorScriptTerm a `Rel` ActorScriptTerm a
  | Rel (ActorScriptTyped Bool)
  | IfThenElse (ActorScriptTerm a) (ActorScriptTerm a) (ActorScriptTyped Bool) -- cond is last!

deriving instance Show a => Show (ActorScriptTerm a)


data ActorScriptTyped :: * -> * where
  NotEqual, Equals, GreaterEqual, Greater, LessEqual, Less
    :: (Show (Neuralgic a), Annot a, Literal a, Evaluatable a) => ActorScriptTerm (Neuralgic a) -> ActorScriptTerm (Neuralgic a) -> ActorScriptTyped Bool
  ShortAnd, ShortOr
    :: ActorScriptTyped Bool -> ActorScriptTyped Bool -> ActorScriptTyped Bool
  Not :: ActorScriptTyped Bool -> ActorScriptTyped Bool
  Bool :: Bool -> ActorScriptTyped Bool
deriving instance Show (ActorScriptTyped t)

subTerm :: Arbitrary (ActorScriptTerm t) => Int -> Bool -> [(Int, Gen (ActorScriptTerm t))]
subTerm n fullPow =
    [(1, resize (n `div` 5) $ Pow <$> arbitrary <*> arbitrary) | fullPow] ++
    [ (n, resize (n `div` 3) $ Add <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Sub <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Mul <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Div <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Mod <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 4) $ IfThenElse <$> arbitrary <*> arbitrary <*> arbitrary)
    ]

subTermPow :: Arbitrary (ActorScriptTerm t) => Int -> (ActorScriptTerm t -> ActorScriptTerm t) -> [(Int, Gen (ActorScriptTerm t))]
subTermPow n mod = (n, resize (n `div` 5) $ Pow <$> arbitrary <*> (mod <$> arbitrary))
                   : subTerm n False

subTermPow5 :: Arbitrary (ActorScriptTerm (Neuralgic t))
               => Int -> [(Int, Gen (ActorScriptTerm (Neuralgic t)))]
subTermPow5 n = (n, resize (n `div` 5)
                      $ Pow <$> arbitrary
                            <*> (About <$> elements [ Around0
                                                    , Around0 `Offset` OneMore
                                                    , AroundPos 1
                                                    , AroundPos 1 `Offset` OneMore
                                                    , AroundPos 2
                                                    ]))
                : subTerm n False

bitwiseTerm :: Arbitrary (ActorScriptTerm t) => Int -> [(Int, Gen (ActorScriptTerm t))]
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
    ]

instance Arbitrary (ActorScriptTerm (Neuralgic Nat8)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    subTerm n True else []

instance Arbitrary (ActorScriptTerm (Neuralgic Nat16)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    subTermPow n (`Mod` Five) else []

instance Arbitrary (ActorScriptTerm (Neuralgic Nat32)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    subTermPow n (`Mod` Five) else []

instance Arbitrary (ActorScriptTerm (Neuralgic Nat64)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    subTermPow n (`Mod` Five) else []


instance Arbitrary (ActorScriptTerm (Neuralgic Int8)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    subTerm n False{-TODO-} else []

instance Arbitrary (ActorScriptTerm (Neuralgic Int16)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    subTermPow5 n else []

instance Arbitrary (ActorScriptTerm (Neuralgic Int32)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    subTermPow5 n else []

instance Arbitrary (ActorScriptTerm (Neuralgic Int64)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    subTermPow5 n else []


instance Arbitrary (ActorScriptTerm (Neuralgic Word8)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    subTerm n True <> bitwiseTerm n else []

instance Arbitrary (ActorScriptTerm (Neuralgic Word16)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    subTermPow n (`Mod` Five) <> bitwiseTerm n else []

instance Arbitrary (ActorScriptTerm (Neuralgic Word32)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    subTermPow n (`Mod` Five) <> bitwiseTerm n else []

instance Arbitrary (ActorScriptTerm (Neuralgic Word64)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    subTermPow n (`Mod` Five) <> bitwiseTerm n else []


instance Arbitrary (ActorScriptTyped Bool) where
  arbitrary = sized $ \(succ -> n) -> -- TODO: use frequency?
    oneof $ (Bool <$> arbitrary) : if n <= 1 then [] else
    [ resize (n `div` 3) $ elements [NotEqual @Integer, Equals, GreaterEqual, Greater, LessEqual, Less] <*> arbitrary <*> arbitrary
    , resize (n `div` 5) $ elements [ShortAnd, ShortOr] <*> arbitrary <*> arbitrary
    , resize (n `div` 2) $ Not <$> arbitrary
    ]


instance Arbitrary (ActorScriptTerm (Neuralgic Natural)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    [ (n, resize (n `div` 3) $ Add <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Sub <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Mul <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Div <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Mod <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 5) $ Pow <$> arbitrary <*> (reasonable <$> arbitrary))] else []
      where reasonable x = Abs x `Mod` Five

instance Arbitrary (ActorScriptTerm (Neuralgic Integer)) where
  arbitrary = sized $ \(succ -> n) -> frequency $
    (30 `div` n, About <$> arbitrary) : if n > 1 then
    [ (n, resize (n `div` 2) $ Pos <$> arbitrary)
    , (n, resize (n `div` 2) $ Neg <$> arbitrary)
    , (n, resize (n `div` 2) $ Abs <$> arbitrary)
    , (n, resize (n `div` 3) $ Add <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Sub <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Mul <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Div <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Mod <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 5) $ Pow <$> arbitrary <*> (reasonable <$> arbitrary))
    , (n, ConvertNatural <$> arbitrary)
    , (n `div` 2, ConvertNat <$> (arbitrary :: Gen (ActorScriptTerm (Neuralgic Nat8))))
    , (n `div` 2, ConvertNat <$> (arbitrary :: Gen (ActorScriptTerm (Neuralgic Nat16))))
    , (n `div` 2, ConvertNat <$> (arbitrary :: Gen (ActorScriptTerm (Neuralgic Nat32))))
    , (n `div` 2, ConvertNat <$> (arbitrary :: Gen (ActorScriptTerm (Neuralgic Nat64))))
    , (n `div` 3, ConvertWord <$> (arbitrary :: Gen (ActorScriptTerm (Neuralgic Word8))))
    , (n `div` 3, ConvertWord <$> (arbitrary :: Gen (ActorScriptTerm (Neuralgic Word16))))
    , (n `div` 3, ConvertWord <$> (arbitrary :: Gen (ActorScriptTerm (Neuralgic Word32))))
    , (n `div` 3, ConvertWord <$> (arbitrary :: Gen (ActorScriptTerm (Neuralgic Word64))))
    , (n `div` 3, ConvertInt <$> (arbitrary :: Gen (ActorScriptTerm (Neuralgic Int8))))
    , (n `div` 3, ConvertInt <$> (arbitrary :: Gen (ActorScriptTerm (Neuralgic Int16))))
    , (n `div` 3, ConvertInt <$> (arbitrary :: Gen (ActorScriptTerm (Neuralgic Int32))))
    , (n `div` 3, ConvertInt <$> (arbitrary :: Gen (ActorScriptTerm (Neuralgic Int64))))
    ] else []
      where reasonable x = Abs x `Mod` Five

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


{-
instance (Eq a, Fractional a) => Fractional (Maybe a) where
  _ / Just 0 = Nothing
  a / b = liftA2 (/) a b
-}

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
guardedEvalN g LargeNeg = g $ toInteger (minBound :: Int) - 111
guardedEvalN g (AroundNeg n) = g $ - 2 ^ n
guardedEvalN g Around0 = g 0
guardedEvalN g (AroundPos n) = g $ 2 ^ n - 1
guardedEvalN g LargePos = g $ fromIntegral (maxBound :: Int) + 333
guardedEvalN g (Offset (guardedEvalN g -> n) (evalO -> o)) = g =<< (o . toInteger) <$> n


class Restricted a where
  substractable :: Maybe a -> Maybe a -> Bool

instance Restricted Integer where substractable _ _ = True
instance Restricted Natural where substractable a b = isJust $ do m <- a; n <- b; guard $ m >= n
instance Restricted Nat8 where substractable a b = isJust $ do NatN m <- a; NatN n <- b; guard $ m >= n


class Integral a => Evaluatable a where
  evaluate :: ActorScriptTerm (Neuralgic a) -> Maybe a


data BitLimited (n :: Nat) (a :: *) where
  NatN :: KnownNat n => Natural -> BitLimited n Natural
  IntN :: KnownNat n => Integer -> BitLimited n Integer
  WordN :: KnownNat n => Natural -> BitLimited n Word

deriving instance Show (BitLimited n a)
deriving instance Eq (BitLimited n a)
deriving instance Ord (BitLimited n a)

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
  evaluate (About n) = NatN <$> trapNat (natVal (Proxy @bits)) (evalN n)
  evaluate ab =
      case ab of
        a `Add` b -> go (+) a b
        a `Sub` b -> go (-) a b
        a `Mul` b -> go (*) a b
        _ `Div` (evaluate -> Just 0) -> Nothing
        a `Div` b -> go quot a b
        _ `Mod` (evaluate -> Just 0) -> Nothing
        a `Mod` b -> go rem a b
        a `Pow` b -> go (^) a b
        IfThenElse a b c -> do c <- evalR c
                               if c then evaluate a else evaluate b
        _ -> error $ show ab
    where go op a b = do NatN a <- evaluate a; NatN b <- evaluate b; NatN <$> trapNat (natVal (Proxy @bits)) (toInteger a `op` toInteger b)

instance KnownNat bits => Evaluatable (BitLimited bits Integer) where
  evaluate Five = pure $ IntN 5
  evaluate (About n) = IntN <$> trapInt (natVal (Proxy @bits)) (evalN n)
  evaluate ab =
      case ab of
        a `Add` b -> go (+) a b
        a `Sub` b -> go (-) a b
        a `Mul` b -> go (*) a b
        _ `Div` (evaluate -> Just 0) -> Nothing
        a `Div` b -> go quot a b
        _ `Mod` (evaluate -> Just 0) -> Nothing
        a `Mod` b -> go rem a b
        a `Pow` b -> go (^) a b
        IfThenElse a b c -> do c <- evalR c
                               if c then evaluate a else evaluate b
        _ -> error $ show ab
    where go op a b = do IntN a <- evaluate a; IntN b <- evaluate b; IntN <$> trapInt (natVal (Proxy @bits)) (toInteger a `op` toInteger b)

instance WordLike bits => Evaluatable (BitLimited bits Word) where
  evaluate Five = pure $ WordN 5
  evaluate (About n) = WordN <$> trapWord (natVal (Proxy @bits)) (evalN n)
  evaluate ab =
      case ab of
        a `Add` b -> go (+) a b
        a `Sub` b -> go (-) a b
        a `Mul` b -> go (*) a b
        _ `Div` (evaluate -> Just 0) -> Nothing
        a `Div` b -> go quot a b
        _ `Mod` (evaluate -> Just 0) -> Nothing
        a `Mod` b -> go rem a b
        a `Pow` b -> go (^) a b
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
        IfThenElse a b c -> do c <- evalR c
                               if c then evaluate a else evaluate b
        _ -> error $ show ab
    where go op a b = do WordN a <- evaluate a; WordN b <- evaluate b; WordN <$> trapWord bitcount (toInteger a `op` toInteger b)
          log op a b = op <$> evaluate a <*> evaluate b
          bitcount = natVal (Proxy @bits)
          signedShiftR a b = fromIntegral $ a' `shiftR` (fromIntegral b `mod` fromIntegral bitcount)
            where a' = toInteger a - (toInteger (((a `rotateL` 1) .&. 1) `rotateR` 1) `shiftL` 1)
instance Evaluatable Integer where
  evaluate = eval
instance Evaluatable Natural where
  evaluate = eval

eval :: (Restricted a, Integral a) => ActorScriptTerm (Neuralgic a) -> Maybe a
eval Five = pure 5
eval (About n) = evalN n
eval (Pos n) = eval n
eval (Neg n) = - eval n
eval (Abs n) = abs $ eval n
eval (a `Add` b) = eval a + eval b
eval ((eval -> a) `Sub` (eval -> b)) = do guard $ substractable a b; a - b
eval (a `Mul` b) = eval a * eval b
eval (a `Div` b) = eval a `quot` eval b
eval (a `Mod` b) = eval a `rem` eval b
eval (a `Pow` b) = do b <- eval b
                      guard $ b >= 0 && b < 5
                      (^) <$> eval a <*> pure b
eval (ConvertNatural t) = fromIntegral <$> evaluate t
eval (ConvertNat t) = fromIntegral <$> evaluate t
eval (ConvertInt t) = fromIntegral <$> evaluate t
eval (ConvertWord t) = fromIntegral <$> evaluate t
eval (IfThenElse a b c) = do c <- evalR c
                             if c then eval a else eval b
eval (Rel r) = bool 0 1 <$> evalR r
--eval _ = Nothing

evalR :: ActorScriptTyped a -> Maybe a
evalR (a `NotEqual` b) = (/=) <$> evaluate a <*> evaluate b
evalR (a `Equals` b) = (==) <$> evaluate a <*> evaluate b
evalR (a `GreaterEqual` b) = (>=) <$> evaluate a <*> evaluate b
evalR (a `Greater` b) = (>) <$> evaluate a <*> evaluate b
evalR (a `LessEqual` b) = (<=) <$> evaluate a <*> evaluate b
evalR (a `Less` b) = (<) <$> evaluate a <*> evaluate b
evalR (a `ShortAnd` b) = evalR a >>= bool (pure False) (evalR b)
evalR (a `ShortOr` b) = evalR a >>= bool (evalR b) (pure True)
evalR (Not a) = not <$> evalR a
evalR (Bool b) = pure b


class Annot t where
  annot :: ActorScriptTerm (Neuralgic t) -> String -> String
  sizeSuffix :: ActorScriptTerm (Neuralgic t) -> String -> String
  sizeSuffix _ = id

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
  literal _ = undefined

showAS :: (Annot a, Literal a) => ActorScriptTerm (Neuralgic a) -> String
showAS f@Five = annot f "5"
showAS a@(About n) = annot a $ literal n
showAS (Pos n) = "(+" <> showAS n <> ")"
showAS (Neg n) = "(-" <> showAS n <> ")"
showAS (Abs n) = "(abs " <> showAS n <> ")"
showAS (a `Add` b) = "(" <> showAS a <> " + " <> showAS b <> ")"
showAS (a `Sub` b) = annot a $ "(" <> showAS a <> " - " <> showAS b <> ")"
showAS (a `Mul` b) = "(" <> showAS a <> " * " <> showAS b <> ")"
showAS (a `Div` b) = "(" <> showAS a <> " / " <> showAS b <> ")"
showAS (a `Mod` b) = "(" <> showAS a <> " % " <> showAS b <> ")"
showAS (a `Pow` b) = "(" <> showAS a <> " ** " <> showAS b <> ")"
showAS (a `Or` b) = "(" <> showAS a <> " | " <> showAS b <> ")"
showAS (a `And` b) = "(" <> showAS a <> " & " <> showAS b <> ")"
showAS (a `Xor` b) = "(" <> showAS a <> " ^ " <> showAS b <> ")"
showAS (a `RotL` b) = "(" <> showAS a <> " <<> " <> showAS b <> ")"
showAS (a `RotR` b) = "(" <> showAS a <> " <>> " <> showAS b <> ")"
showAS (a `ShiftL` b) = "(" <> showAS a <> " << " <> showAS b <> ")"
showAS (a `ShiftR` b) = "(" <> showAS a <> " >> " <> showAS b <> ")"
showAS (a `ShiftRSigned` b) = "(" <> showAS a <> " +>> " <> showAS b <> ")"
showAS (PopCnt n) = sizeSuffix n "(popcntWord" <> " " <> showAS n <> ")"
showAS (Clz n) = sizeSuffix n "(clzWord" <> " " <> showAS n <> ")"
showAS (Ctz n) = sizeSuffix n "(ctzWord" <> " " <> showAS n <> ")"
showAS (ConvertNatural a) = "(++++(" <> showAS a <> "))"
showAS (ConvertNat a) = showNat Proxy a
showAS (ConvertInt a) = showInt Proxy a
showAS (ConvertWord a) = showWord Proxy a
showAS (Rel r) = showBool r
showAS (IfThenElse a b c) = "(if (" <> showBool c <> ") " <> showAS a <> " else " <> showAS b <> ")"

showBool :: ActorScriptTyped Bool -> String
showBool (a `NotEqual` b) = "(" <> showAS a <> " != " <> showAS b <> ")" -- for now!
showBool (a `Equals` b) = "(" <> showAS a <> " == " <> showAS b <> ")" -- for now!
showBool (a `GreaterEqual` b) = "(" <> showAS a <> " >= " <> showAS b <> ")" -- for now!
showBool (a `Greater` b) = "(" <> showAS a <> " > " <> showAS b <> ")" -- for now!
showBool (a `LessEqual` b) = "(" <> showAS a <> " <= " <> showAS b <> ")" -- for now!
showBool (a `Less` b) = "(" <> showAS a <> " < " <> showAS b <> ")" -- for now!
showBool (a `ShortAnd` b) = "(" <> showBool a <> " and " <> showBool b <> ")" -- for now!
showBool (a `ShortOr` b) = "(" <> showBool a <> " or " <> showBool b <> ")" -- for now!
showBool (Not a) = "(not " <> showBool a <> ")" -- for now!
showBool (Bool False) = "false"
showBool (Bool True) = "true"

showNat :: KnownNat n => Proxy n -> ActorScriptTerm (Neuralgic (BitLimited n Natural)) -> String
showNat p a = "(nat" <> bitWidth p <> "ToNat(" <> showAS a <> "))"

showInt :: KnownNat n => Proxy n -> ActorScriptTerm (Neuralgic (BitLimited n Integer)) -> String
showInt p a = "(int" <> bitWidth p <> "ToInt(" <> showAS a <> "))"

showWord :: KnownNat n => Proxy n -> ActorScriptTerm (Neuralgic (BitLimited n Word)) -> String
showWord p a = "(word" <> bitWidth p <> "ToNat(" <> showAS a <> "))" -- TODO we want signed too: wordToInt



-- TODOs:
--   - wordToInt
--   - Pow for Int8
--   - bitwise ops (btst?)
--   - bitwise not, a.k.a unary (^)
--   - understand this: suchThat not called above?
--   - pattern matches (over numeric, bool)
