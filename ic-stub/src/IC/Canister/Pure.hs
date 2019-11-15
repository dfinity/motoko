{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wmissing-signatures #-}
{-|
A “pure” implementation of canisters, using "IC.Canister.Imp", but just replaying when needed.
Note that this is quadratic in the number of update calls, so do not run such canisters for long.

We could do some hacking caching of state using stable names, so that as long as no trap occurs, 'replay' is fast.
-}

module IC.Canister.Pure
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
import IC.Canister.Imp

data WasmState
  = WSInit Module CanisterId EntityId Blob
  | WSUpdate MethodName EntityId Blob WasmState
  | WSCallback Callback EntityId Response WasmState

initializeMethod :: Module -> CanisterId -> EntityId -> Blob -> TrapOr WasmState
initializeMethod wasm_mod cid caller dat = runESST $ \esref ->
  rawInitializeModule esref wasm_mod >>= \case
    Trap err -> return $ Trap err
    Return inst -> do
      result <- rawInitializeMethod esref wasm_mod inst cid caller dat
      let state' = WSInit wasm_mod cid caller dat
      case result of
        Trap err          -> return $ Trap err
        Return _raw_state -> return $ Return state'

updateMethod ::
    MethodName -> EntityId -> Blob ->
    WasmState -> TrapOr (WasmState, [MethodCall], Maybe Response)
updateMethod m caller dat s = runESST $ \esref -> do
  rs <- replay esref s
  tor <- rawUpdateMethod rs m caller dat
  let state' = WSUpdate m caller dat s
  case tor of
    Trap msg -> return $ Trap msg
    Return (calls, r) -> return $ Return (state', calls, r)

callbackMethod ::
    Callback -> EntityId -> Response ->
    WasmState -> TrapOr (WasmState, [MethodCall], Maybe Response)
callbackMethod cb caller r s = runESST $ \esref -> do
  rs <- replay esref s
  tor <- rawCallbackMethod rs cb caller r
  let state' = WSCallback cb caller r s
  case tor of
    Trap msg -> return $ Trap msg
    Return (calls, r) -> return $ Return (state', calls, r)

queryMethod :: MethodName -> EntityId -> Blob -> WasmState -> TrapOr Response
queryMethod m caller dat s = runESST $ \esref -> do
  rs <- replay esref s
  rawQueryMethod rs m caller dat


replay :: forall s. ESRef s -> WasmState -> ST s (ImpState s)
replay esref s = silently esref $ go s
  where
    trapToFail (Trap _err) = fail "replay failed"
    trapToFail (Return x) = return x

    go :: WasmState -> ST s (ImpState s)
    go (WSInit wasm_mod cid caller dat) = do
      inst <- rawInitializeModule esref wasm_mod >>= trapToFail
      rawInitializeMethod esref wasm_mod inst cid caller dat >>= trapToFail
      return (esref, cid, inst)
    go (WSUpdate m caller dat s) = do
      is <- go s
      _ <- rawUpdateMethod is m caller dat >>= trapToFail
      return is
    go (WSCallback cb caller r s) = do
      is <- go s
      _ <- rawCallbackMethod is cb caller r >>= trapToFail
      return is
