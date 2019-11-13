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

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSU
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Text.Lazy as T
import Data.List
import Data.Monoid
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Except
import Control.Monad.ST
import Data.STRef
import Data.Binary.Get (runGetOrFail)
import Data.Default.Class (Default (..))

import IC.Types
import IC.Wasm.Winter (parseModule, exportedFunctions, Module)
import IC.Canister.Imp

data WasmState
  = WSInit Module CanisterId Blob
  | WSUpdate MethodName Arg WasmState

type InitFunc = (CanisterId, Blob) -> TrapOr WasmState
type UpdateFunc = WasmState -> TrapOr (WasmState, {- List MethodCall, -} Maybe Response)
type QueryFunc = WasmState -> TrapOr Response

data CanisterModule = CanisterModule
  { init_method :: InitFunc
  , update_methods :: MethodName ↦ (Arg -> UpdateFunc)
  , query_methods :: MethodName ↦ (Arg -> QueryFunc)
  }

parseCanister :: Blob -> Either String CanisterModule
parseCanister bytes =
  case parseModule bytes of
    Left  err -> Left err
    Right wasm_mod -> Right $ concreteToAbstractModule wasm_mod

concreteToAbstractModule :: Module -> CanisterModule
concreteToAbstractModule wasm_mod = CanisterModule
  { init_method = \(cid, arg) -> initializeMethod wasm_mod cid arg
  , update_methods = M.fromList
    [ (m, \arg wasm_state -> updateMethod m arg wasm_state)
    | n <- exportedFunctions wasm_mod
    , Just m <- return $ stripPrefix "canister_update " n
    ]
  , query_methods = M.fromList
    [ (m, \arg wasm_state -> queryMethod m arg wasm_state)
    | n <- exportedFunctions wasm_mod
    , Just m <- return $ stripPrefix "canister_query " n
    ]
  }

initializeMethod :: Module -> CanisterId -> Blob -> TrapOr WasmState
initializeMethod wasm_mod cid arg = runESST $ \esref -> do
  result <- rawInitializeMethod esref wasm_mod cid arg
  case result of
    Trap err          -> return $ Trap err
    Return _raw_state -> return $ Return $ WSInit wasm_mod cid arg

updateMethod :: MethodName -> Arg -> WasmState -> TrapOr (WasmState, Maybe Response)
updateMethod m arg s = runESST $ \esref -> do
  rs <- replay esref s
  tor <- rawUpdateMethod rs m arg
  case tor of
    Trap msg -> return $ Trap msg
    Return r -> return $ Return (WSUpdate m arg s, r)

queryMethod :: MethodName -> Arg -> WasmState -> TrapOr Response
queryMethod m arg s = runESST $ \esref -> do
  rs <- replay esref s
  rawQueryMethod rs m arg


replay :: ESRef s -> WasmState -> ST s (RawState s)
replay esref s = silently esref $ go s
  where
    trapToFail (Trap err) = fail "replay failed"
    trapToFail (Return x) = return x

    go (WSInit wasm_mod cid arg) =
      rawInitializeMethod esref wasm_mod cid arg >>= trapToFail
    go (WSUpdate m arg s) = do
      rs <- go s
      _ <- rawUpdateMethod rs m arg >>= trapToFail
      return rs

