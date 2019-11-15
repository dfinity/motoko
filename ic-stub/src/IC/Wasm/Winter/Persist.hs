{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
This module provides a way to persist the state of a Winter Wasm instance, and
to recover it.

It is tailored to the use by ic-stub. For example it assumes that the
table of a wasm instance is immutable.
-}
module IC.Wasm.Winter.Persist where

import Control.Monad.Identity
import Control.Monad.ST
import Data.Primitive.MutVar
import qualified Data.IntMap as IM
import qualified Data.Map.Lazy as M
import qualified Data.Text.Lazy as T
import Data.Word
import qualified Data.Vector.Unboxed as V

import qualified Wasm.Runtime.Global as W
import qualified Wasm.Runtime.Instance as W
import qualified Wasm.Runtime.Memory as W
import qualified Wasm.Syntax.Values as W

type Instance s = (IM.IntMap (W.ModuleInst Identity (ST s)), Int)

-- |
-- This stores data read from an instance.
--
-- Note that an 'Instance' has aliasing, the same global may appear in various
-- spots. We don’t worry about that for now and just de-alias on reading, at the
-- expense of writing the corresponding mutable values more than once.
newtype PInstance = PInstance (IM.IntMap PModuleInst)

persistInstance :: Instance s -> ST s PInstance
persistInstance (i,_) = PInstance <$> persist1 i


class Persistable f where
  type Persisted f :: *
  persist :: f (ST s) -> ST s (Persisted f)
  resume :: f (ST s) -> Persisted f -> ST s ()

instance Persistable W.MemoryInst where
  type Persisted W.MemoryInst = V.Vector Word8
  persist m = readMutVar (W._miContent m) >>= V.freeze
  resume m v = V.thaw v >>= writeMutVar (W._miContent m)

instance Persistable W.GlobalInst where
  type Persisted W.GlobalInst = W.Value
  persist m = readMutVar (W._giContent m)
  resume m = writeMutVar (W._giContent m)


data PExtern
  = PExternMemory (Persisted W.MemoryInst)
  | PExternGlobal (Persisted W.GlobalInst)
  | PExternOther

instance Persistable (W.Extern f) where
  type Persisted (W.Extern f) = PExtern

  persist (W.ExternGlobal g) = PExternGlobal <$> persist g
  persist (W.ExternMemory m) = PExternMemory <$> persist m
  persist _ = return PExternOther

  resume (W.ExternGlobal g) (PExternGlobal pg) = resume g pg
  resume (W.ExternMemory m) (PExternMemory pm) = resume m pm
  resume _ _ = return ()

data PModuleInst = PModuleInst
  { memories :: [Persisted W.MemoryInst]
  , globals :: [Persisted W.GlobalInst]
  , exports :: M.Map T.Text (Persisted (W.Extern Identity))
  }

instance Persistable (W.ModuleInst Identity) where
  type Persisted (W.ModuleInst Identity) = PModuleInst
  persist inst = PModuleInst
    <$> persist1 (W._miMemories inst)
    <*> persist1 (W._miGlobals inst)
    <*> persist1 (W._miExports inst)
  resume inst pinst = do
    resume1 (W._miMemories inst) (memories pinst)
    resume1 (W._miGlobals inst) (globals pinst)
    resume1 (W._miExports inst) (exports pinst)


class Persistable1 c f where
  persist1 :: c (f (ST s)) -> ST s (c (Persisted f))
  resume1 :: c (f (ST s)) -> c (Persisted f) -> ST s ()

instance Persistable f => Persistable1 [] f where
  persist1 = mapM persist
  resume1 xs ys = do
    unless (length xs == length ys) $ fail "Lengths don’t match"
    zipWithM_ resume xs ys

instance (Eq k, Persistable f) => Persistable1 (M.Map k) f where
  persist1 = mapM persist
  resume1 xs ys = do
    unless (M.keys xs == M.keys ys) $ fail "Map keys don’t match"
    zipWithM_ resume (M.elems xs) (M.elems ys)

instance Persistable f => Persistable1 IM.IntMap f where
  persist1 = mapM persist
  resume1 xs ys = do
    unless (IM.keys xs == IM.keys ys) $ fail "Map keys don’t match"
    zipWithM_ resume (IM.elems xs) (IM.elems ys)
