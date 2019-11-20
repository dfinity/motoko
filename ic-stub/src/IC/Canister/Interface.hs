{-# LANGUAGE GADTs #-}

{-|
This module defines the various entry points provided by "IC.Canister.Imp" as a data type describing the arguments and results.

The point of this abstraction is to to allow adding new entry points, or changing the arguments or result values, without changing the modules "IC.Canister.Pure" or "IC.Canister.Persisted", that sit between "IC.Canister.Imp" and "IC.Canister".
-}
module IC.Canister.Interface where

import IC.Types
import IC.Wasm.Winter (Module)

type UpdateResult = ([MethodCall], Maybe Response)

data CanisterMethod r where
    Initialize :: Module -> EntityId -> Blob -> CanisterMethod ()
    Query :: MethodName -> EntityId -> Blob -> CanisterMethod Response
    Update :: MethodName -> EntityId -> Blob -> CanisterMethod UpdateResult
    Callback :: Callback -> EntityId -> Response -> CanisterMethod UpdateResult
