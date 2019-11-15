{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wmissing-signatures #-}
{-|
An implementation of canisters based on persistence, using "IC.Canister.Imp". It has the same interface as "IC.Canister.Pure".
-}

module IC.Canister.Persisted
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
import IC.Wasm.Winter.Persist
import IC.Canister.Imp

data WasmState = WasmState Module CanisterId PInstance

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
