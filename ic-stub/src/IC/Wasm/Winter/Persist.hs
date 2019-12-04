{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
This module provides a way to persist the state of a Winter Wasm instance, and
to recover it.

It is tailored to the use by ic-stub. For example it assumes that the
table of a wasm instance is immutable.
-}
module IC.Wasm.Winter.Persist
  ( PInstance
  , persistInstance
  , resumeInstance
  )
  where

import Control.Monad.Identity
import Control.Monad.ST
import Data.Primitive.MutVar
import qualified Data.IntMap as IM
import qualified Data.Map.Lazy as M
import qualified Data.Text.Lazy as T
import Data.ByteString.Lazy (ByteString)

import qualified Wasm.Runtime.Global as W
import qualified Wasm.Runtime.Instance as W
import qualified Wasm.Runtime.Memory as W
import qualified Wasm.Syntax.Values as W

import IC.Wasm.Winter (Instance)

-- |
-- This stores data read from an instance.
--
-- Note that an 'Instance' has aliasing, the same global may appear in various
-- spots. We don’t worry about that for now and just de-alias on reading, at the
-- expense of writing the corresponding mutable values more than once.
newtype PInstance = PInstance (Persisted (Instance ()))

persistInstance :: Instance s -> ST s PInstance
persistInstance i = PInstance <$> persist i

resumeInstance :: Instance s -> PInstance -> ST s ()
resumeInstance i (PInstance p) = resume i p

class Monad (M a) => Persistable a where
  type Persisted a :: *
  type M a :: * -> *
  persist :: a -> M a (Persisted a)
  resume :: a -> Persisted a -> M a ()

instance Persistable (W.MemoryInst (ST s)) where
  type Persisted (W.MemoryInst (ST s)) = ByteString
  type M (W.MemoryInst (ST s)) = ST s
  persist = W.exportMemory
  resume = W.importMemory

instance Persistable (W.GlobalInst (ST s)) where
  type Persisted (W.GlobalInst (ST s)) = W.Value
  type M (W.GlobalInst (ST s)) = ST s
  persist m = readMutVar (W._giContent m)
  resume m = writeMutVar (W._giContent m)

data PExtern
  = PExternMemory (Persisted (W.MemoryInst (ST ())))
  | PExternGlobal (Persisted (W.GlobalInst (ST ())))
  | PExternOther

instance Persistable (W.Extern f (ST s)) where
  type Persisted (W.Extern f (ST s)) = PExtern
  type M (W.Extern f (ST s)) = ST s

  persist (W.ExternGlobal g) = PExternGlobal <$> persist g
  persist (W.ExternMemory m) = PExternMemory <$> persist m
  persist _ = return PExternOther

  resume (W.ExternGlobal g) (PExternGlobal pg) = resume g pg
  resume (W.ExternMemory m) (PExternMemory pm) = resume m pm
  resume _ _ = return ()

data PModuleInst = PModuleInst
  { memories :: [Persisted (W.MemoryInst (ST ()))]
  , globals :: [Persisted (W.GlobalInst (ST ()))]
  , exports :: M.Map T.Text (Persisted (W.Extern Identity (ST ())))
  }

instance Persistable (W.ModuleInst Identity (ST s)) where
  type Persisted (W.ModuleInst Identity (ST s)) = PModuleInst
  type M (W.ModuleInst Identity (ST s)) = ST s
  persist inst = PModuleInst
    <$> persist (W._miMemories inst)
    <*> persist (W._miGlobals inst)
    <*> persist (W._miExports inst)
  resume inst pinst = do
    resume (W._miMemories inst) (memories pinst)
    resume (W._miGlobals inst) (globals pinst)
    resume (W._miExports inst) (exports pinst)


instance Persistable a => Persistable [a] where
  type Persisted [a] = [Persisted a]
  type M [a] = M a
  persist = mapM persist
  resume xs ys = do
    unless (length xs == length ys) $ fail "Lengths don’t match"
    zipWithM_ resume xs ys

instance (Eq k, Persistable a) => Persistable (M.Map k a) where
  type Persisted (M.Map k a) = M.Map k (Persisted a)
  type M (M.Map k a) = M a
  persist = mapM persist
  resume xs ys = do
    unless (M.keys xs == M.keys ys) $ fail "Map keys don’t match"
    zipWithM_ resume (M.elems xs) (M.elems ys)

instance Persistable a => Persistable (IM.IntMap a) where
  type Persisted (IM.IntMap a) = IM.IntMap (Persisted a)
  type M (IM.IntMap a) = M a
  persist = mapM persist
  resume xs ys = do
    unless (IM.keys xs == IM.keys ys) $ fail "Map keys don’t match"
    zipWithM_ resume (IM.elems xs) (IM.elems ys)

instance Persistable a => Persistable (a, Int) where
  type Persisted (a, Int) = Persisted a
  type M (a, Int) = M a
  persist (a, _i) = persist a
  resume (a, _i) p = resume a p
