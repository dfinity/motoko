{-# LANGUAGE ScopedTypeVariables #-}
{-|

This module provides a thin wrapper around the winter Wasm engine, exposing just
the bits needed for accessing the stable memory. This will hopefully go away once
multiple memories are supported.
-}
module IC.Wasm.WinterMemory
  ( Memory
  , new
  , size
  , grow
  , read
  , write
  , export
  , imp
  )
where

import Prelude hiding (read)

import Data.ByteString.Lazy (ByteString)
import Control.Monad.Except
import Control.Monad.ST

import qualified Wasm.Runtime.Memory as W
import qualified Wasm.Syntax.Types as W
import qualified Wasm.Syntax.Memory as W

type HostM s = ExceptT String (ST s)
type Memory s = W.MemoryInst (ST s)

new :: HostM s (Memory s)
new = withExceptT show $ W.alloc (W.Limits 0 Nothing)

size :: Memory s -> HostM s W.Size
size mem = lift $ W.size mem

grow :: Memory s -> W.Size -> HostM s W.Size
grow mem delta = lift $ do
    -- See memory.grow impl in src/Wasm/Exec/Eval.hs
  oldSize <- W.size mem
  eres    <- runExceptT $ W.grow mem delta
  return $ case eres of
      Left _   -> -1
      Right () -> oldSize

read :: Memory s -> W.Address -> W.Size -> HostM s ByteString
read mem ptr len = withExceptT show $ W.loadBytes mem ptr len

write :: Memory s -> W.Address -> ByteString -> HostM s ()
write mem ptr blob = withExceptT show $ W.storeBytes mem (fromIntegral ptr) blob

export :: Memory s -> ST s ByteString
export = W.exportMemory

imp :: Memory s -> ByteString -> ST s ()
imp = W.importMemory
