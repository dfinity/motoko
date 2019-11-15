{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wmissing-signatures #-}
{-|
An implementation of canisters based on persistence, using "IC.Canister.Imp". It has the same interface as "IC.Canister.Pure".
-}

module IC.Canister.Persisted
    ( WasmState
    , initializeMethod
    , updateMethod
    , callbackMethod
    , queryMethod
    )
    where

import Control.Monad.ST

import IC.Types
import IC.Wasm.Winter (Module)
import IC.Wasm.Winter.Persist
import IC.Canister.Imp

data WasmState = WasmState Module CanisterId PInstance

initializeMethod :: Module -> CanisterId -> EntityId -> Blob -> TrapOr WasmState
initializeMethod wasm_mod cid caller dat = runESST $ \esref ->
  rawInitializeModule esref wasm_mod >>= \case
    Trap err -> return $ Trap err
    Return inst ->
      rawInitializeMethod esref wasm_mod inst cid caller dat >>= \case
        Trap err -> return $ Trap err
        Return () -> Return . WasmState wasm_mod cid <$> persistInstance inst

updateMethod ::
    MethodName -> EntityId -> Blob ->
    WasmState -> TrapOr (WasmState, [MethodCall], Maybe Response)
updateMethod m caller dat s = runESST $ \esref -> do
  rs <- replay esref s
  tor <- rawUpdateMethod rs m caller dat
  case tor of
    Trap msg -> return $ Trap msg
    Return (calls, r) -> do
      state' <- persist s rs
      return $ Return (state', calls, r)

callbackMethod ::
    Callback -> EntityId -> Response ->
    WasmState -> TrapOr (WasmState, [MethodCall], Maybe Response)
callbackMethod cb caller r s = runESST $ \esref -> do
  rs <- replay esref s
  tor <- rawCallbackMethod rs cb caller r
  case tor of
    Trap msg -> return $ Trap msg
    Return (calls, r) -> do
      state' <- persist s rs
      return $ Return (state', calls, r)

queryMethod :: MethodName -> EntityId -> Blob -> WasmState -> TrapOr Response
queryMethod m caller dat s = runESST $ \esref -> do
  rs <- replay esref s
  rawQueryMethod rs m caller dat


replay :: ESRef s -> WasmState -> ST s (ImpState s)
replay esref (WasmState wasm_mod cid pinst) = do
    is <- rawInitializeModule esref wasm_mod >>= trapToFail
    resumeInstance is pinst
    return (esref, cid, is)
  where
    trapToFail (Trap _err) = fail "replay failed"
    trapToFail (Return x) = return x

persist :: WasmState -> ImpState s -> ST s WasmState
persist (WasmState wasm_mod cid _) (_, _, inst) =
  WasmState wasm_mod cid <$> persistInstance inst
