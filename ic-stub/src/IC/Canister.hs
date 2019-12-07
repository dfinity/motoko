{-# LANGUAGE TypeOperators #-}

{-|
A “pure” implementation of canisters, using "IC.Canister.Imp", but just replaying when needed.
Note that this is quadratic in the number of update calls, so do not run such canisters for long.

We could do some hacking caching of state using stable names, so that, as long as no trap occurs, 'replay' is fast.
-}

module IC.Canister
    ( WasmState
    , parseCanister
    , CanisterModule(..)
    , InitFunc, UpdateFunc, QueryFunc
    )
    where

import qualified Data.Map as M
import Data.List

import IC.Types
import IC.Wasm.Winter (parseModule, exportedFunctions, Module)

import qualified IC.Canister.Interface as CI
-- Here we can swap out the persistence implementation
import IC.Canister.Persisted

type InitFunc = ExistingCanisters -> CanisterId -> EntityId -> Blob -> TrapOr (InitResult, WasmState)
type UpdateFunc = WasmState -> TrapOr (WasmState, UpdateResult)
type QueryFunc = WasmState -> TrapOr Response

data CanisterModule = CanisterModule
  { init_method :: InitFunc
  , update_methods :: MethodName ↦ (ExistingCanisters -> EntityId -> Blob -> UpdateFunc)
  , query_methods :: MethodName ↦ (EntityId -> Blob -> QueryFunc)
  , callbacks :: Callback -> ExistingCanisters -> Response -> UpdateFunc
  }

parseCanister :: Blob -> Either String CanisterModule
parseCanister bytes =
  case parseModule bytes of
    Left  err -> Left err
    Right wasm_mod -> Right $ concreteToAbstractModule wasm_mod

concreteToAbstractModule :: Module -> CanisterModule
concreteToAbstractModule wasm_mod = CanisterModule
  { init_method = \ex cid caller dat -> initialize ex wasm_mod cid caller dat
  , update_methods = M.fromList
    [ (m, \ex caller dat wasm_state -> invoke wasm_state (CI.Update m ex caller dat))
    | n <- exportedFunctions wasm_mod
    , Just m <- return $ stripPrefix "canister_update " n
    ]
  , query_methods = M.fromList
    [ (m, \caller arg wasm_state ->
        snd <$> invoke wasm_state (CI.Query m caller arg))
    | n <- exportedFunctions wasm_mod
    , Just m <- return $ stripPrefix "canister_query " n
    ]
  , callbacks = \cb ex res wasm_state ->
    invoke wasm_state (CI.Callback cb ex res)
  }
