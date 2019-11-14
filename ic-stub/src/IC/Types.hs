{-# LANGUAGE TypeOperators #-}
module IC.Types where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Data.Int

type (↦) = M.Map

-- Basic types

type Blob = BS.ByteString
newtype EntityId = EntityId { rawEntityId :: Blob } deriving (Show, Eq, Ord)
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

rejectCode :: RejectCode -> Int
rejectCode RC_SYS_FATAL           = 1
rejectCode RC_SYS_TRANSIENT       = 2
rejectCode RC_DESTINATION_INVALID = 3
rejectCode RC_CANISTER_REJECT     = 4
rejectCode RC_CANISTER_ERROR      = 5


data Response = Reply Blob | Reject (RejectCode, String)
  deriving Show

-- Abstract canisters

data TrapOr a = Trap String | Return a

data WasmClosure = WasmClosure
  { closure_idx :: Int32
  , closure_env :: Int32
  }
  deriving Show

data Callback = Callback
  { reply_callback :: WasmClosure
  , reject_callback :: WasmClosure
  }
  deriving Show

data MethodCall = MethodCall
  { call_callee :: CanisterId
  , call_method_name :: MethodName
  , call_arg :: Blob
  , call_callback :: Callback
  }
  deriving Show
