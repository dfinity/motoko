{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wmissing-signatures #-}
{-|
A “pure” implementation of canisters, using "IC.Canister.Imp", but just replaying when needed.
Note that this is quadratic in the number of update calls, so do not run such canisters for long.

We could do some hacking caching of state using stable names, so that as long as no trap occurs, 'replay' is fast.
-}

module IC.Canister.Pure
    ( WasmState
    , initialize
    , invoke
    )
    where

import Control.Monad.ST

import IC.Types
import IC.Wasm.Winter (Module)
import qualified IC.Canister.Interface as CI
import IC.Canister.Imp

data ACall = forall r. ACall (CI.CanisterMethod r)

data WasmState = WasmState
    { ws_mod :: Module
    , ws_self_id :: CanisterId
    , ws_calls :: [ACall] -- in reverse order
    }

initialize :: Module -> ExistingCanisters -> CanisterId -> EntityId -> Blob -> TrapOr WasmState
initialize wasm_mod ex cid caller dat = runESST $ \esref ->
  rawInitialize esref cid wasm_mod >>= \case
    Trap err -> return $ Trap err
    Return rs -> do
      let m = CI.Initialize ex wasm_mod caller dat
      result <- rawInvoke rs m
      let state' = WasmState wasm_mod cid [ACall m]
      case result of
        Trap err          -> return $ Trap err
        Return _raw_state -> return $ Return state'

invoke :: WasmState -> CI.CanisterMethod r -> TrapOr (WasmState, r)
invoke s m = runESST $ \esref -> do
  rs <- replay esref s
  tor <- rawInvoke rs m
  let state' = s { ws_calls = ACall m : ws_calls s }
  case tor of
    Trap msg -> return $ Trap msg
    Return r -> return $ Return (state', r)

replay :: forall s. ESRef s -> WasmState -> ST s (ImpState s)
replay esref WasmState{ ws_mod, ws_self_id, ws_calls } = silently esref $ go ws_calls
  where
    trapToFail (Trap _err) = fail "replay failed"
    trapToFail (Return x) = return x

    go :: [ACall] -> ST s (ImpState s)
    go [] = rawInitialize esref ws_self_id ws_mod >>= trapToFail
    go (ACall m:ms) = do
      is <- go ms
      _ <- rawInvoke is m >>= trapToFail
      return is
