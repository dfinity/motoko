{-# language ConstraintKinds, DataKinds, DeriveFoldable, FlexibleContexts, FlexibleInstances, GADTs
           , KindSignatures, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving
           , TypeApplications, TypeOperators, TypeFamilies, TupleSections
           , UndecidableInstances, ViewPatterns #-}

{-# options_ghc -Wno-missing-methods #-}

module Main where

import Control.Applicative
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding ((.&.))
import Test.QuickCheck.Unicode
import System.Environment
import qualified Data.Text (null, pack, unpack)
import Data.Maybe
import Data.Bool (bool)
import Data.Proxy
import Data.Type.Equality
import GHC.Natural
import GHC.TypeLits hiding (Text)
import qualified Data.Word
import Data.Bits (Bits(..), FiniteBits(..))
import Numeric
import Data.List (intercalate)

import Turtle
import Embedder
-- import Debug.Trace (traceShowId, traceShow)


main = do
  putStrLn "Probing embedders..."
  good <- isHealthy embedder
  goodDrun <- isHealthy Drun
  putStrLn . ("Found " <>) . (<> ".") . intercalate " and " . map embedderCommand
               $ [ embedder | good ] <> [ Drun | goodDrun ]
  let tests :: TestTree
      tests = testGroup "Motoko tests" . concat
               $ [ [arithProps, conversionProps, utf8Props, matchingProps] | good ]
              <> [ [encodingProps] | goodDrun ]

  if not (good || goodDrun)
  then putStrLn "No embedder available for testing. Done..."
  else setEnv "TASTY_NUM_THREADS" "1" >> defaultMain tests

arithProps = testGroup "Arithmetic/logic"
  [ QC.testProperty "expected failures" $ prop_rejects
  , QC.testProperty "expected successes" $ prop_verifies
  ]

conversionProps = testGroup "Numeric conversions"
  [ QC.testProperty "roundtrip Word64 Nat Word64 " $ prop_roundtripWNW @64
  , QC.testProperty "roundtrip Word32 Nat Word32 " $ prop_roundtripWNW @32
  , QC.testProperty "roundtrip Word16 Nat Word16 " $ prop_roundtripWNW @16
  , QC.testProperty "roundtrip Word8 Nat Word8 " $ prop_roundtripWNW @8
  , QC.testProperty "modulo Nat Word64 Nat" $ prop_moduloNWN @64
  , QC.testProperty "modulo Nat Word32 Nat" $ prop_moduloNWN @32
  , QC.testProperty "modulo Nat Word16 Nat" $ prop_moduloNWN @16
  , QC.testProperty "modulo Nat Word8 Nat" $ prop_moduloNWN @8
  ]

utf8Props = testGroup "UTF-8 coding"
  [ QC.testProperty "explode >>> concat roundtrips" $ prop_explodeConcat
  , QC.testProperty "charToText >>> head roundtrips" $ prop_charToText
  , QC.testProperty "length computation" $ prop_textLength
  , QC.testProperty "chunky concat (ropes)" $ prop_ropeConcat
  , QC.testProperty "chunky length (ropes)" $ prop_ropeLength
  , QC.testProperty "chunky iterator (ropes)" $ prop_ropeIterator
  ]

matchingProps = testGroup "Pattern matching" $
  [ QC.testProperty "intra-actor" $ prop_matchStructured ]


-- these require messaging
--
encodingProps = testGroup "Encoding" $
  [ QC.testProperty "inter-actor" $ withMaxSuccess 20 prop_matchInActor
  , QC.testProperty "encoded-Nat" $ withMaxSuccess 10 prop_matchActorNat
  , QC.testProperty "encoded-Int" $ withMaxSuccess 10 prop_matchActorInt
  , QC.testProperty "encoded-Text" $ withMaxSuccess 10 prop_matchActorText
  ]


withPrim :: Line -> Line
withPrim = (fromString "import Prim \"mo:prim\";" <>)

runner :: Embedder -> ExitCode -> (Bool -> Bool) -> Turtle.FilePath -> String -> PropertyM IO ()
runner embedder reqOutcome relevant name testCase =
    let as = name <.> "mo"
        wasm = name <.> "wasm"
        fileArg = fromString . encodeString
        script = do Turtle.output as $ withPrim <$> fromString testCase
                    res@(exitCode, _, _) <- procStrictWithErr "moc"
                      (addCompilerArgs embedder ["-no-check-ir", fileArg as]) empty
                    if ExitSuccess == exitCode
                    then (True,) <$> invokeEmbedder embedder wasm
                    else pure (False, res)
    in run script >>= assertOutcomeCheckingFuzz reqOutcome relevant

(runScriptNoFuzz, runScriptWantFuzz) = (runEmbedder ExitSuccess id, runEmbedder (ExitFailure 134) not)
    where runEmbedder = runner embedder
(drunScriptNoFuzz, drunScriptWantFuzz) = (runEmbedder ExitSuccess id, runEmbedder (ExitFailure 1) not)
    where runEmbedder = runner Drun

prop_explodeConcat :: UTF8 String -> Property
prop_explodeConcat (UTF8 str) = monadicIO $ do
  let testCase :: String
      testCase = "{ var str = \"\"; for (c in \""
                 <> s <> "\".chars()) { str #= Prim.charToText c }; assert (str == \"" <> s <> "\") }"

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
  let testCase = "assert (switch ((Prim.charToText '"
                 <> c <> "').chars().next()) { case (?'" <> c <> "') true; case _ false })"

      c = escape char
  runScriptNoFuzz "charToText" testCase

prop_textLength (UTF8 text) = monadicIO $ do
  let testCase = "assert(\"" <> (text >>= escape) <> "\".size() == " <> show (length text) <> ")"
  runScriptNoFuzz "textLength" testCase

data Rope a = EmptyChunk | Chunk a | UTF8Chunk a | LongChunk a | Rope a `Rope` Rope a deriving (Eq, Show, Foldable)

instance Semigroup (Rope a) where
  (<>) = Rope

instance Monoid (Rope a) where
  mempty = EmptyChunk

instance Arbitrary (Rope String) where
  arbitrary = frequency [ (2, pure mempty)
                        , (4, Chunk <$> arbitrary)
                        , (5, elements [ UTF8Chunk "Медве́ди хо́дят по у́лицам. Коне́чно, э́то непра́вда!"
                                       , UTF8Chunk "Boci, boci tarka, se füle, se farka, oda megyünk lakni, ahol tejet kapni."
                                       , UTF8Chunk "十年树木，百年树人" ])
                        , (7, pure $ LongChunk "The quick brown fox jumps over the lazy dog")
                        , (3, Rope <$> arbitrary <*> arbitrary) ]

instance Semigroup (MOTerm String) where
  (<>) = Concat

instance Monoid (MOTerm String) where
  mempty = Text mempty

asString = foldMap id

asMot :: Rope String -> MOTerm String
asMot = foldMap Text

prop_ropeConcat rope = monadicIO $ do
  let testCase = "assert (" <> ropeMot <> " == " <> string <> ")"
      string = unparseMO (Text (asString rope))
      ropeMot = unparseMO (asMot rope)
  runScriptNoFuzz "ropeConcat" testCase

prop_ropeLength rope = monadicIO $ do
  let testCase = "assert (" <> ropeMot <> ".size() == " <> show len <> ")"
      len = length (asString rope)
      ropeMot = unparseMO (asMot rope)
  runScriptNoFuzz "ropeLength" testCase

prop_ropeIterator rope = monadicIO $ do
  let testCase = "func same(c : ?Char, d : Char) : Bool = switch c { case (?cc) { cc == d }; case null false };"
              <> "let i = (" <> ropeMot <> ").chars();"
              <> concatMap testStep string
              <> "assert (switch (i.next()) { case null true; case _ false })"
      testStep c = "assert (same(i.next(), '" <> escape c <> "'));"
      string = asString rope
      ropeMot = unparseMO (asMot rope)
  runScriptNoFuzz "ropeLength" testCase


assertOutcomeCheckingFuzz outcome relevant (compiled, (exitCode, out, err)) = do
  let fuzzErr = not $ Data.Text.null err
      fuzzErrRelevant = relevant fuzzErr
  when (fuzzErr && fuzzErrRelevant) $ do
    monitor (counterexample "STDERR:")
    monitor (counterexample . Data.Text.unpack $ err)
  let fuzzOut = not $ Data.Text.null out
  when fuzzOut $ do
    monitor (counterexample "STDOUT:")
    monitor (counterexample . Data.Text.unpack $ out)
  assert (not $ outcome /= exitCode || (if compiled then fuzzErrRelevant else fuzzErr) || fuzzOut)

assertSuccessNoFuzz = assertOutcomeCheckingFuzz ExitSuccess id

newtype Failing a = Failing a deriving Show

instance Arbitrary (Failing String) where
  arbitrary = do let failed as = "let _ = " ++ unparseMO as ++ ";"
                 Failing . failed <$> suchThat (resize 5 arbitrary) (\(evaluate @Integer -> res) -> null res)

prop_rejects (Failing testCase) = monadicIO $ runScriptWantFuzz "fails" testCase

halve [] = ([], [])
halve a@[_] = (a, [])
halve (clown : joker : (halve -> (cs, js))) = (clown : cs, joker : js)

newtype TestCase = TestCase [String] deriving Show

instance Arbitrary TestCase where
  arbitrary = do tests <- infiniteListOf arbitrary
                 let expected = evaluate @Integer <$> tests
                 let paired as = fmap (\res -> "assert (" ++ unparseMO as ++ " == " ++ show res ++ ");")
                 pure . TestCase . take 10 . catMaybes $ zipWith paired tests expected


prop_verifies (TestCase (map fromString -> testCase)) = monadicIO $ do
  let script cases = do Turtle.output "tests.mo" $ msum (pure (withPrim mempty) : cases)
                        res@(exitCode, _, _) <- procStrictWithErr "moc"
                                 (addCompilerArgs embedder ["-no-check-ir", "tests.mo"]) empty
                        if ExitSuccess == exitCode
                        then (True,) <$> procStrictWithErr (embedderCommand embedder) (addEmbedderArgs embedder ["tests.wasm"]) empty
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
  assertSuccessNoFuzz res


newtype ConversionTest n = ConversionTest (MOTerm (BitLimited n Word)) deriving Show

instance WordLike n => Arbitrary (ConversionTest n) where
  arbitrary = ConversionTest . ConvertNatToWord . ConvertWordToNat . Neuralgic <$> (arbitrary :: Gen (Neuralgic (BitLimited n Word)))

prop_roundtripWNW :: ConversionTest n -> Property
prop_roundtripWNW (ConversionTest term) =
    case term of
      ConvertNatToWord (ConvertWordToNat n) ->
          case sameNat (bitsIn term) (bitsIn n) of
            Just Refl -> let testCase = "assert(" <> unparseMO (term `Equals` n) <> ")" in
                         monadicIO $ runScriptNoFuzz "roundtripWNW" testCase
  where bitsIn :: KnownNat n => MOTerm (BitLimited n Word) -> Proxy n
        bitsIn _ = Proxy


newtype ModuloTest (n :: Nat) = ModuloTest (MOTerm (BitLimited n Word)) deriving Show

instance WordLike n => Arbitrary (ModuloTest n) where
  arbitrary = ModuloTest . ConvertNatToWord @n . Neuralgic <$> (arbitrary :: Gen (Neuralgic Natural))

prop_moduloNWN :: forall n . KnownNat n => ModuloTest n -> Property
prop_moduloNWN (ModuloTest term@(ConvertNatToWord (Neuralgic m))) = monadicIO $ runScriptNoFuzz "moduloNWN" testCase
  where m' = evalN m .&. maskFor term
        testCase = "assert(" <> unparseMO (ConvertWordToNat term)
                <> " == " <> show m' <> ")"

data Matching where
  Matching :: (AnnotLit t, MOValue t, Show t) => (MOTerm t, t) -> Matching

deriving instance Show Matching


instance Arbitrary Matching where
  arbitrary = oneof [ realise Matching <$> gen @Bool
                    , realise Matching <$> gen @(Bool, Bool)
                    , realise Matching <$> gen @(Bool, Integer)
                    , realise Matching <$> gen @((Bool, Natural), Integer)
                    , realise Matching <$> gen @(BitLimited 8 Natural, BitLimited 8 Integer, BitLimited 8 Word)
                    , realise Matching <$> gen @(Maybe Integer)
                    ]
    where gen :: (Arbitrary (MOTerm a), Evaluatable a) => Gen (MOTerm a, Maybe a)
          gen = (do term <- arbitrary
                    let val = evaluate term
                    pure (term, val)) `suchThat` (isJust . snd)
          realise f (tm, Just v) = f (tm, v)

prop_matchStructured :: Matching -> Property
prop_matchStructured (Matching a) = locally a

locally :: (AnnotLit t, MOValue t) => (MOTerm t, t) -> Property
locally (tm, v) = monadicIO $ do
  let testCase = "assert (switch (" <> expr <> ") { case (" <> eval'd <> ") true; case _ false })"

      eval'd = unparse v
      expr = unparseMO tm
  runScriptNoFuzz "matchLocally" testCase

prop_matchInActor :: Matching -> Property
prop_matchInActor (Matching a) = mobile a

mobile :: (AnnotLit t, MOValue t) => (MOTerm t, t) -> Property
mobile (tm, v) = monadicIO $ do
  let testCase = "actor { public func match (b : " <> typed <> ") : async () { assert (switch b { case (" <> eval'd <> ") true; case _ false }) }; public func do () : async () { let res = await match (" <> expr <> " : " <> typed <> "); return res } };"

      eval'd = unparse v
      typed = unparseType v
      expr = unparseMO tm
  drunScriptNoFuzz "matchMobile" testCase


prop_matchActorNat :: Neuralgic Natural -> Property
prop_matchActorNat nat = monadicIO $ do
    let testCase = format ("actor { public func match (n : Nat) : async () { assert (switch n { case ("%d%") true; case _ false }) }; public func do () : async () { let res = await match ("%d%" : Nat); return res } };") eval'd eval'd
        eval'd = evalN nat
    drunScriptNoFuzz "matchActorNat" (Data.Text.unpack testCase)

prop_matchActorInt :: Neuralgic Integer -> Property
prop_matchActorInt int = monadicIO $ do
    let testCase = format ("actor { public func match (i : Int) : async () { assert (switch i { case ("%d%") true; case _ false }) }; public func do () : async () { let res = await match ("%d%" : Int); return res } };") eval'd eval'd
        eval'd = evalN int
    drunScriptNoFuzz "matchActorInt" (Data.Text.unpack testCase)

prop_matchActorText :: UTF8 String -> Property
prop_matchActorText (UTF8 text) = monadicIO $ do
    let testCase = format ("actor { public func match (i : Text) : async () { assert (switch i { case (\""%s%"\") true; case _ false }) }; public func do () : async () { let res = await match (\""%s%"\" : Text); return res } };") eval'd eval'd
        eval'd = Data.Text.pack $ text >>= escape
    drunScriptNoFuzz "matchActorText" (Data.Text.unpack testCase)

-- instances of MOValue describe "ground values" in
-- Motoko. These can appear in patterns and have
-- well-defined Motoko type.
--
class MOValue a where
  unparseType :: a -> String
  unparse :: a -> String

instance MOValue Bool where
  unparseType _ = "Bool"
  unparse = unparseMO . Bool

instance MOValue Integer where
  unparseType _ = "Int"
  unparse = show

instance MOValue Natural where
  unparseType _ = "Nat"
  unparse = show

instance KnownNat n => MOValue (BitLimited n Natural) where
  unparseType _ = "Nat" <> bitWidth (Proxy @n)
  unparse (NatN a) = annot (Five @(BitLimited n Natural)) (show a)

instance KnownNat n => MOValue (BitLimited n Integer) where
  unparseType _ = "Int" <> bitWidth (Proxy @n)
  unparse (IntN a) = annot (Five @(BitLimited n Integer)) (show a)

instance KnownNat n => MOValue (BitLimited n Word) where
  unparseType _ = "Word" <> bitWidth (Proxy @n)
  unparse (WordN a) = annot (Five @(BitLimited n Word)) (show a)

instance (MOValue a, MOValue b) => MOValue (a, b) where
  unparseType (a, b) = "(" <> unparseType a <> ", " <> unparseType b <> ")"
  unparse (a, b) = "(" <> unparse a <> ", " <> unparse b <> ")"

instance (MOValue a, MOValue b, MOValue c) => MOValue (a, b, c) where
  unparseType (a, b, c) = "(" <> unparseType a <> ", " <> unparseType b <> ", " <> unparseType c <> ")"
  unparse (a, b, c) = "(" <> unparse a <> ", " <> unparse b <> ", " <> unparse c <> ")"

instance MOValue a => MOValue (Maybe a) where
  unparseType Nothing = "Null"
  unparseType (Just a) = "?" <> unparseType a
  unparse Nothing = "null"
  unparse (Just a) = "?" <> unparse a

-- wiggle room around an *important value*
-- think of it as a "fuzz"
--
data Off = TwoLess | OneLess | OneMore | TwoMore
 deriving (Enum, Eq, Ord, Show)

instance Arbitrary Off where
  arbitrary = elements [TwoLess .. TwoMore]

-- Below data structure tries to focus test case generation of
-- numeric values to *neuralgic points* of the numeric line, namely
-- areas where (from our white-box knowledge of Motoko's inner
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
               `guardedFrom` [Around0, AroundPos 30, AroundPos 63, LargePos, AroundPos 77]

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


data MOTerm :: * -> * where
  -- Comparisons
  NotEqual, Equals, GreaterEqual, Greater, LessEqual, Less
    :: (AnnotLit a, Evaluatable a) => MOTerm a -> MOTerm a -> MOTerm Bool
  -- Short-circuit
  ShortAnd, ShortOr
    :: MOTerm Bool -> MOTerm Bool -> MOTerm Bool
  -- Boolean
  Not :: MOTerm Bool -> MOTerm Bool
  Bool :: Bool -> MOTerm Bool
  -- Bitwise
  Complement :: MOTerm (BitLimited n Word) -> MOTerm (BitLimited n Word)
  Or, And, Xor, RotL, RotR, ShiftL, ShiftR, ShiftRSigned
    :: MOTerm (BitLimited n Word) -> MOTerm (BitLimited n Word) -> MOTerm (BitLimited n Word)
  PopCnt, Clz, Ctz :: MOTerm (BitLimited n Word) -> MOTerm (BitLimited n Word)
  -- Arithmetic
  Pos, Neg, Abs :: MOTerm a -> MOTerm a
  Add, Sub, Mul, Div, Mod, Pow :: MOTerm a -> MOTerm a -> MOTerm a
  -- Numeric
  Neuralgic :: Neuralgic a -> MOTerm a
  Five :: MOTerm a
  -- Text
  Text :: String -> MOTerm String
  Concat :: MOTerm String -> MOTerm String -> MOTerm String
  -- Conditional
  IfThenElse :: MOTerm a -> MOTerm a -> MOTerm Bool -> MOTerm a
  -- Conversion
  ConvertNatural :: MOTerm Natural -> MOTerm Integer
  ConvertNat :: KnownNat n => MOTerm (BitLimited n Natural) -> MOTerm Integer
  ConvertInt :: KnownNat n => MOTerm (BitLimited n Integer) -> MOTerm Integer
  ConvertWord :: WordLike n => MOTerm (BitLimited n Word) -> MOTerm Integer
  ConvertNatToWord :: WordLike n => MOTerm Natural -> MOTerm (BitLimited n Word)
  ConvertWordToNat :: WordLike n => MOTerm (BitLimited n Word) -> MOTerm Natural
  -- Constructors (intro forms)
  Pair :: (AnnotLit a, AnnotLit b, Evaluatable a, Evaluatable b) => MOTerm a -> MOTerm b -> MOTerm (a, b)
  Triple :: (AnnotLit a, AnnotLit b, AnnotLit c, Evaluatable a, Evaluatable b, Evaluatable c)
         => MOTerm a -> MOTerm b -> MOTerm c -> MOTerm (a, b, c)
  Array :: MOTerm a -> MOTerm [a] -- not matchable!
  Null :: MOTerm (Maybe a)
  Some :: (AnnotLit a, Evaluatable a) => MOTerm a -> MOTerm (Maybe a)
  -- Variants, Objects (TODO)

deriving instance Show (MOTerm t)

subTerm :: Arbitrary (MOTerm t) => Bool -> Int -> [(Int, Gen (MOTerm t))]
subTerm fullPow n =
    [ (1, resize (n `div` 5) $ Pow <$> arbitrary <*> arbitrary) | fullPow] ++
    [ (n, resize (n `div` 3) $ Add <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Sub <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Mul <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Div <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 3) $ Mod <$> arbitrary <*> arbitrary)
    , (n, resize (n `div` 4) $ IfThenElse <$> arbitrary <*> arbitrary <*> arbitrary)
    ]

subTermPow :: Arbitrary (MOTerm t) => (MOTerm t -> MOTerm t) -> Int -> [(Int, Gen (MOTerm t))]
subTermPow mod n = (n, resize (n `div` 5) $ Pow <$> arbitrary <*> (mod <$> arbitrary))
                   : subTerm False n

subTermPow5 :: Arbitrary (MOTerm t)
               => Int -> [(Int, Gen (MOTerm t))]
subTermPow5 n = (n, resize (n `div` 5)
                      $ Pow <$> arbitrary
                            <*> (Neuralgic <$> elements [ Around0
                                                    , Around0 `Offset` OneMore
                                                    , AroundPos 1
                                                    , AroundPos 1 `Offset` OneMore
                                                    , AroundPos 2
                                                    ]))
                : subTerm False n

bitwiseTerm :: WordLike n => Arbitrary (MOTerm (BitLimited n Word)) => Int -> [(Int, Gen (MOTerm (BitLimited n Word)))]
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
reasonablyShaped :: (Arbitrary (Neuralgic a), AnnotLit a, Evaluatable a)
                 => (Int -> [(Int, Gen (MOTerm a))])
                 -> Gen (MOTerm a)
reasonablyShaped sub = sized $ \(succ -> n) -> frequency $
                       (30 `div` n, Neuralgic <$> arbitrary)
                       : if n > 1 then sub n else []

instance {-# OVERLAPPABLE #-} KnownNat n => Arbitrary (MOTerm (BitLimited n Natural)) where
  arbitrary = reasonablyShaped $ subTermPow (`Mod` Five)

instance {-# OVERLAPS #-} Arbitrary (MOTerm Nat8) where
  arbitrary = reasonablyShaped $ subTerm True


instance {-# OVERLAPPABLE #-} KnownNat n => Arbitrary (MOTerm (BitLimited n Integer)) where
  arbitrary = reasonablyShaped subTermPow5

instance {-# OVERLAPS #-} Arbitrary (MOTerm Int8) where
  arbitrary = reasonablyShaped $ subTerm True


instance {-# OVERLAPPABLE #-} WordLike n => Arbitrary (MOTerm (BitLimited n Word)) where
  arbitrary = reasonablyShaped $ (<>) <$> subTermPow (`Mod` Five) <*> bitwiseTerm

instance {-# OVERLAPS #-} Arbitrary (MOTerm Word8) where
  arbitrary = reasonablyShaped $ (<>) <$> subTerm True <*> bitwiseTerm

instance (AnnotLit a, AnnotLit b, Evaluatable a, Evaluatable b, Arbitrary (MOTerm a), Arbitrary (MOTerm b)) => Arbitrary (MOTerm (a, b)) where
  arbitrary = scale (`quot` 2) $ Pair <$> arbitrary <*> arbitrary

instance (AnnotLit a, AnnotLit b, AnnotLit c, Evaluatable a, Evaluatable b, Evaluatable c, Arbitrary (MOTerm a), Arbitrary (MOTerm b), Arbitrary (MOTerm c))
    => Arbitrary (MOTerm (a, b, c)) where
  arbitrary = scale (`quot` 3) $ Triple <$> arbitrary <*> arbitrary <*> arbitrary

instance (AnnotLit a, Evaluatable a, Arbitrary (MOTerm a)) => Arbitrary (MOTerm (Maybe a)) where
  arbitrary = frequency [(1, pure Null), (10, Some <$> arbitrary)]

instance Arbitrary (MOTerm Bool) where
  arbitrary = sized $ \(succ -> n) -> -- TODO: use frequency?
    oneof $ (Bool <$> arbitrary) : if n <= 1 then [] else
    [ resize (n `div` 3) $ elements [NotEqual @Integer, Equals, GreaterEqual, Greater, LessEqual, Less] <*> arbitrary <*> arbitrary
    , resize (n `div` 5) $ elements [ShortAnd, ShortOr] <*> arbitrary <*> arbitrary
    , resize (n `div` 2) $ Not <$> arbitrary
    ]

instance Arbitrary (MOTerm Natural) where
  arbitrary = reasonablyShaped $ subTermPow (`Mod` Five)

instance Arbitrary (MOTerm Integer) where
  arbitrary = reasonablyShaped $ \n ->
    [ (n, resize (n `div` 2) $ Pos <$> arbitrary)
    , (n, resize (n `div` 2) $ Neg <$> arbitrary)
    , (n, resize (n `div` 2) $ Abs <$> arbitrary)
    , (n, ConvertNatural <$> arbitrary)
    , (n `div` 2, ConvertNat <$> (arbitrary @(MOTerm Nat8)))
    , (n `div` 2, ConvertNat <$> (arbitrary @(MOTerm Nat16)))
    , (n `div` 2, ConvertNat <$> (arbitrary @(MOTerm Nat32)))
    , (n `div` 2, ConvertNat <$> (arbitrary @(MOTerm Nat64)))
    , (n `div` 3, ConvertWord <$> (arbitrary @(MOTerm Word8)))
    , (n `div` 3, ConvertWord <$> (arbitrary @(MOTerm Word16)))
    , (n `div` 3, ConvertWord <$> (arbitrary @(MOTerm Word32)))
    , (n `div` 3, ConvertWord <$> (arbitrary @(MOTerm Word64)))
    , (n `div` 3, ConvertInt <$> (arbitrary @(MOTerm Int8)))
    , (n `div` 3, ConvertInt <$> (arbitrary @(MOTerm Int16)))
    , (n `div` 3, ConvertInt <$> (arbitrary @(MOTerm Int32)))
    , (n `div` 3, ConvertInt <$> (arbitrary @(MOTerm Int64)))
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
  evaluate :: MOTerm a -> Maybe a


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

instance (Evaluatable a, Evaluatable b, Evaluatable c) => Evaluatable (a, b, c) where
  evaluate (Triple a b c) = (,,) <$> evaluate a <*> evaluate b <*> evaluate c

instance Evaluatable a => Evaluatable (Maybe a) where
  evaluate Null = pure Nothing
  evaluate (Some a) = Just <$> evaluate a

maskFor :: forall n . KnownNat n => MOTerm (BitLimited n Word) -> Natural
maskFor _ = fromIntegral $ 2 ^ natVal (Proxy @n) - 1


eval :: (Restricted a, Integral a) => MOTerm a -> Maybe a
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
eval c@(ConvertNatToWord t) = fromIntegral . (.&. maskFor c) <$> evaluate t
eval (ConvertWordToNat t) = fromIntegral <$> evaluate t
eval (IfThenElse a b c) = do c <- evaluate c
                             eval $ if c then a else b
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

instance Evaluatable String where
  evaluate (Text a) = pure a
  evaluate (a `Concat` b) = (<>) <$> evaluate a <*> evaluate b

type AnnotLit t = (Annot t, Literal t)

class Annot t where
  annot :: MOTerm t -> String -> String
  sizeSuffix :: MOTerm t -> String -> String
  sizeSuffix _ = id

instance Annot (a, b) where
  annot _ = id

instance Annot (a, b, c) where
  annot _ = id

instance Annot (Maybe a) where
  annot _ = id

instance Annot Integer where
  annot _ s = "((" <> s <> ") : Int)"

instance Annot Natural where
  annot _ s = "((" <> s <> ") : Nat)"

instance Annot String where
  annot _ = id

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

instance Literal (a, b, c) where
  literal _ = error "Literal (a, b, c) makes no sense"

instance Literal (Maybe a) where
  literal _ = error "Literal (Maybe a) makes no sense"

instance Literal String where
  literal _ = error "Literal String makes no sense"

instance Literal Integer
instance Literal Natural
instance Literal (BitLimited n Natural)
instance KnownNat bits => Literal (BitLimited bits Integer) where
  literal (evalN -> n) = if n < 0
                         then "(" <> show n <> ")"
                         else show n
instance KnownNat bits => Literal (BitLimited bits Word) where
  literal n = show . fromJust $ trapWord (natVal (Proxy @bits)) (evalN n)

instance Literal Bool where
  literal _ = error "Literal Bool makes no sense"

inParens :: (a -> String) -> String -> a -> a -> String
inParens to op lhs rhs = "(" <> to lhs <> " " <> op <> " " <> to rhs <> ")"

unparseMO :: AnnotLit a => MOTerm a -> String
unparseMO f@Five = annot f "5"
unparseMO a@(Neuralgic n) = annot a $ literal n
unparseMO (Pos n) = "(+" <> unparseMO n <> ")"
unparseMO (Neg n) = "(-" <> unparseMO n <> ")"
unparseMO (Abs n) = "(Prim.abs " <> unparseMO n <> ")"
unparseMO (a `Add` b) = inParens unparseMO "+" a b
unparseMO (a `Sub` b) = annot a $ inParens unparseMO "-" a b
unparseMO (a `Mul` b) = inParens unparseMO "*" a b
unparseMO (a `Div` b) = inParens unparseMO "/" a b
unparseMO (a `Mod` b) = inParens unparseMO "%" a b
unparseMO (a `Pow` b) = inParens unparseMO "**" a b
unparseMO (a `Or` b) = inParens unparseMO "|" a b
unparseMO (a `And` b) = inParens unparseMO "&" a b
unparseMO (a `Xor` b) = inParens unparseMO "^" a b
unparseMO (a `RotL` b) = inParens unparseMO "<<>" a b
unparseMO (a `RotR` b) = inParens unparseMO "<>>" a b
unparseMO (a `ShiftL` b) = inParens unparseMO "<<" a b
unparseMO (a `ShiftR` b) = inParens unparseMO ">>" a b
unparseMO (a `ShiftRSigned` b) = inParens unparseMO "+>>" a b
unparseMO (PopCnt n) = sizeSuffix n "(Prim.popcntWord" <> " " <> unparseMO n <> ")"
unparseMO (Clz n) = sizeSuffix n "(Prim.clzWord" <> " " <> unparseMO n <> ")"
unparseMO (Ctz n) = sizeSuffix n "(Prim.ctzWord" <> " " <> unparseMO n <> ")"
unparseMO (Complement a) = "(^ " <> unparseMO a <> ")"
unparseMO (ConvertNatural a) = "(++++(" <> unparseMO a <> "))"
unparseMO (ConvertNat a) = unparseNat Proxy a
unparseMO (ConvertInt a) = unparseInt Proxy a
unparseMO (ConvertWord a) = unparseWord Proxy a
unparseMO (ConvertWordToNat a) = sizeSuffix a "(Prim.word" <> "ToNat " <> unparseMO a <> ")"
unparseMO t@(ConvertNatToWord a) = sizeSuffix t "(Prim.natToWord" <> " " <> unparseMO a <> ")"
unparseMO (IfThenElse a b c) = "(if (" <> unparseMO c <> ") " <> unparseMO a <> " else " <> unparseMO b <> ")"
unparseMO (a `NotEqual` b) = inParens unparseMO "!=" a b
unparseMO (a `Equals` b) = inParens unparseMO "==" a b
unparseMO (a `GreaterEqual` b) = inParens unparseMO ">=" a b
unparseMO (a `Greater` b) = inParens unparseMO ">" a b
unparseMO (a `LessEqual` b) = inParens unparseMO "<=" a b
unparseMO (a `Less` b) = inParens unparseMO "<" a b
unparseMO (a `ShortAnd` b) = inParens unparseMO "and" a b
unparseMO (a `ShortOr` b) = inParens unparseMO "or" a b
unparseMO (Not a) = "(not " <> unparseMO a <> ")"
unparseMO (Bool False) = "false"
unparseMO (Bool True) = "true"
unparseMO (a `Pair` b) = "(" <> unparseMO a <> ", " <> unparseMO b <> ")"
unparseMO (Triple a b c) = "(" <> unparseMO a <> ", " <> unparseMO b <> ", " <> unparseMO c <> ")"
unparseMO Null = "null"
unparseMO (Some a) = '?' : unparseMO a
unparseMO (Text a) = '"' : (concatMap escape a) <> "\""
unparseMO (a `Concat` b) = "(" <> unparseMO a <> " # " <> unparseMO b <> ")"

unparseNat :: KnownNat n => Proxy n -> MOTerm (BitLimited n Natural) -> String
unparseNat p a = "(Prim.nat" <> bitWidth p <> "ToNat(" <> unparseMO a <> "))"

unparseInt :: KnownNat n => Proxy n -> MOTerm (BitLimited n Integer) -> String
unparseInt p a = "(Prim.int" <> bitWidth p <> "ToInt(" <> unparseMO a <> "))"

unparseWord :: KnownNat n => Proxy n -> MOTerm (BitLimited n Word) -> String
unparseWord p a = "(Prim.word" <> bitWidth p <> "ToNat(" <> unparseMO a <> "))" -- TODO we want signed too: wordToInt

-- TODOs:
--   - wordToInt
--   - natToNat64/intToWord64 (and round-trips)
--       Done: natToWordN/wordNToNat
--   - bitwise ops (btst?)
--   - pattern matches (over numeric, bool, structured)
--   - trapping flavour-preserving conversions Nat -> NatN
--   - bitsize-preserving conversions
--   - data MOTerm (p :: {Term, Pattern}) where ... Pattern :: MOValue a => a -> MOTerm (Pattern/both) a
--   - Text and Concat tests (partly covered by rope tests)
