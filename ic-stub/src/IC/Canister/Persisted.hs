{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wmissing-signatures #-}
{-|
An implementation of canisters based on persistence, using "IC.Canister.Imp". It has the same interface as "IC.Canister.Pure".
-}

module IC.Canister.Persisted
    ( WasmState
    , initialize
    , initializeUpgrade
    , invoke
    )
    where

import Control.Monad.ST
import Data.ByteString.Lazy (ByteString)

import IC.Types
import IC.Wasm.Winter (Module)
import IC.Wasm.Winter.Persist
import qualified IC.Canister.Interface as CI
import IC.Canister.Imp

data WasmState = WasmState Module CanisterId PInstance ByteString

initialize :: Module -> CanisterId -> EntityId -> Blob -> TrapOr (InitResult, WasmState)
initialize wasm_mod cid caller dat = runESST $ \esref ->
  rawInitialize esref cid wasm_mod >>= \case
    Trap err -> return $ Trap err
    Return rs ->
      rawInvoke rs (CI.Initialize wasm_mod caller dat) >>= \case
        Trap err -> return $ Trap err
        Return ir -> Return . (ir,) <$> newWasmState wasm_mod rs

initializeUpgrade :: Module -> CanisterId -> EntityId -> Blob -> Blob -> TrapOr WasmState
initializeUpgrade wasm_mod cid caller mem dat = runESST $ \esref ->
  rawInitialize esref cid wasm_mod >>= \case
    Trap err -> return $ Trap err
    Return rs ->
      rawInvoke rs (CI.PostUpgrade wasm_mod caller mem dat) >>= \case
        Trap err -> return $ Trap err
        Return () -> Return <$> newWasmState wasm_mod rs

invoke :: WasmState -> CI.CanisterMethod r -> TrapOr (WasmState, r)
invoke s m = runESST $ \esref -> do
  rs <- replay esref s
  tor <- rawInvoke rs m
  case tor of
    Trap msg -> return $ Trap msg
    Return r -> do
      state' <- persist s rs
      return $ Return (state', r)

replay :: ESRef s -> WasmState -> ST s (ImpState s)
replay esref (WasmState wasm_mod cid pinst pmem) = do
    rs <- rawInitialize esref cid wasm_mod >>= trapToFail
    resume rs pinst pmem
    return rs
  where
    trapToFail (Trap _err) = fail "replay failed"
    trapToFail (Return x) = return x

newWasmState :: Module -> ImpState s -> ST s WasmState
newWasmState wasm_mod (ImpState _ cid inst sm) =
  WasmState wasm_mod cid <$> persistInstance inst <*> persistMemory sm

resume :: ImpState s -> PInstance -> ByteString -> ST s ()
resume (ImpState _  _ inst sm) pinst pmem = do
  resumeInstance inst pinst
  resumeMemory sm pmem

persist :: WasmState -> ImpState s -> ST s WasmState
persist (WasmState wasm_mod cid _ _) (ImpState _ _ inst sm) =
  WasmState wasm_mod cid <$> persistInstance inst <*> persistMemory sm
