{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wmissing-signatures #-}
{-|
A “pure” implementation of canisters, using "IC.Canister.Imp", but just replaying when needed.
Note that this is quadratic in the number of update calls, so do not run such canisters for long.

We could do some hacking caching of state using stable names, so that as long as no trap occurs, 'replay' is fast.
-}

module IC.Canister.Pure
    ( WasmState
    , parseCanister
    , CanisterModule(..)
    , InitFunc, UpdateFunc, QueryFunc
    )
    where

import qualified Data.Map as M
import Data.List
import Control.Monad.ST

import IC.Types
import IC.Wasm.Winter (parseModule, exportedFunctions, Module)
import IC.Canister.Imp

data WasmState
  = WSInit Module CanisterId EntityId Blob
  | WSUpdate MethodName EntityId Blob WasmState
  | WSCallback Callback EntityId Response WasmState

type InitFunc = CanisterId -> EntityId -> Blob -> TrapOr WasmState
type UpdateFunc = WasmState -> TrapOr (WasmState, [MethodCall], Maybe Response)
type QueryFunc = WasmState -> TrapOr Response

data CanisterModule = CanisterModule
  { init_method :: InitFunc
  , update_methods :: MethodName ↦ (EntityId -> Blob -> UpdateFunc)
  , query_methods :: MethodName ↦ (EntityId -> Blob -> QueryFunc)
  , callbacks :: Callback -> EntityId -> Response -> UpdateFunc
  }

parseCanister :: Blob -> Either String CanisterModule
parseCanister bytes =
  case parseModule bytes of
    Left  err -> Left err
    Right wasm_mod -> Right $ concreteToAbstractModule wasm_mod

concreteToAbstractModule :: Module -> CanisterModule
concreteToAbstractModule wasm_mod = CanisterModule
  { init_method = \cid caller dat -> initializeMethod wasm_mod cid caller dat
  , update_methods = M.fromList
    [ (m, \caller dat wasm_state -> updateMethod m caller dat wasm_state)
    | n <- exportedFunctions wasm_mod
    , Just m <- return $ stripPrefix "canister_update " n
    ]
  , query_methods = M.fromList
    [ (m, \arg wasm_state -> queryMethod m arg wasm_state)
    | n <- exportedFunctions wasm_mod
    , Just m <- return $ stripPrefix "canister_query " n
    ]
  , callbacks = callbackMethod
  }

initializeMethod :: Module -> CanisterId -> EntityId -> Blob -> TrapOr WasmState
initializeMethod wasm_mod cid caller dat = runESST $ \esref -> do
  result <- rawInitializeMethod esref wasm_mod cid caller dat
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


replay :: ESRef s -> WasmState -> ST s (RawState s)
replay esref s = silently esref $ go s
  where
    trapToFail (Trap _err) = fail "replay failed"
    trapToFail (Return x) = return x

    go (WSInit wasm_mod cid caller dat) =
      rawInitializeMethod esref wasm_mod cid caller dat >>= trapToFail
    go (WSUpdate m caller dat s) = do
      rs <- go s
      _ <- rawUpdateMethod rs m caller dat >>= trapToFail
      return rs
    go (WSCallback cb caller r s) = do
      rs <- go s
      _ <- rawCallbackMethod rs cb caller r >>= trapToFail
      return rs

