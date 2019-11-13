{-# LANGUAGE TypeOperators #-}
module IC.Types where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M

type (↦) = M.Map

-- Basic types

type Blob = BS.ByteString
type EntityId = Blob
type CanisterId = EntityId
type MethodName = String
type RequestID = Blob

data RejectCode
    = RC_SYS_FATAL
    | RC_SYS_TRANSIENT
    | RC_DESTINATION_INVALID
    | RC_CANISTER_REJECT
    | RC_CANISTER_ERROR
  deriving Show

data Response = Reply Blob | Reject (RejectCode, String)

-- Abstract canisters

data TrapOr a = Trap String | Return a

data Arg = Arg
  { dat :: Blob
  , caller :: EntityId
  }
  deriving Show
