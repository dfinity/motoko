{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wmissing-signatures #-}
module IC.Stub where

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
import IC.Canister.Pure

rejectCode :: RejectCode -> Int
rejectCode RC_SYS_FATAL           = 1
rejectCode RC_SYS_TRANSIENT       = 2
rejectCode RC_DESTINATION_INVALID = 3
rejectCode RC_CANISTER_REJECT     = 4
rejectCode RC_CANISTER_ERROR      = 5

-- Abstract HTTP Interface

data AsyncRequest
    = InstallRequest Blob Blob
    | UpdateRequest MethodName Blob
  deriving (Eq, Ord, Show)

data SyncRequest
    = QueryRequest MethodName Blob
    | StatusRequest Blob

data RequestStatus
  = Unknown -- never inside IC, only as ReqResponse
  | Received
  | Processing
  | Rejected (RejectCode, String)
  | Completed CompletionValue
  deriving Show

type ReqResponse = RequestStatus


data CompletionValue -- we need to be more typed than the public spec here
  = CompleteUnit
  | CompleteCanisterId CanisterId
  | CompleteArg Blob
  deriving Show

-- Abstract canisters

-- IC state

data CanState = CanState
  { wasm_state :: WasmState
  , can_mod :: CanisterModule
  }

type ICT = MonadState IC

data IC = IC
  { canisters :: CanisterId ↦ Maybe CanState
  , requests :: AsyncRequest ↦ RequestStatus
  }

initialIC :: IC
initialIC = IC mempty mempty

submitRequest :: ICT m => AsyncRequest -> m ()
submitRequest r =
  -- lensify?
  modify (\ic -> ic { requests = M.insert r Received (requests ic) })

requestId :: AsyncRequest -> Blob
requestId r = BSC.pack (show r) -- TODO: Implement request hashing

findRequest :: RequestID -> IC -> Maybe (AsyncRequest, RequestStatus)
findRequest rid ic = find (\(r,s) -> requestId r == rid) (M.toList (requests ic))

readRequest :: ICT m => SyncRequest -> m ReqResponse

readRequest (StatusRequest rid) =
  gets (findRequest rid) >>= \case
    Just (r,status) -> return status
    Nothing -> return Unknown

readRequest (QueryRequest method arg) =
  gets (M.lookup dummyCanisterId . canisters) >>= \case
    Nothing -> return $ Rejected (RC_DESTINATION_INVALID, "canister does not exist")
    Just Nothing -> return $ Rejected (RC_DESTINATION_INVALID, "canister is empty")
    Just (Just (CanState wasm_state can_mod)) ->
      case M.lookup method (query_methods can_mod) of
        Nothing -> return $ Rejected (RC_DESTINATION_INVALID, "query method does not exist")
        Just f ->
          case f (Arg arg dummyUserId) wasm_state of
            Trap msg -> return $ Rejected (RC_CANISTER_ERROR, "canister trapped: " ++ msg)
            Return (Reply res) -> return $ Completed (CompleteArg res)
            Return (Reject (rc,rm)) -> return $ Rejected (rc, rm)

nextRecieved :: ICT m => m (Maybe AsyncRequest)
nextRecieved = gets $ \ic -> listToMaybe
  [ r | (r, Received) <- M.toList (requests ic) ]

nextStarved :: ICT m => m (Maybe AsyncRequest)
nextStarved = gets $ \ic -> listToMaybe
  -- TODO: This should return a starved calling context.
  -- Until we have them, we treat requests in Processing state as starved
  [ r | (r, Processing) <- M.toList (requests ic) ]

setReqStatus :: ICT m => AsyncRequest -> RequestStatus -> m ()
setReqStatus r s =
  modify (\ic -> ic { requests = M.insert r s (requests ic) })

createCanister :: ICT m => CanisterId -> CanisterModule -> WasmState -> m ()
createCanister cid can_mod wasm_state =
  modify (\ic -> ic { canisters =
    M.insert cid (Just (CanState {can_mod, wasm_state})) (canisters ic)
  })

setCanisterState :: ICT m => CanisterId -> WasmState -> m ()
setCanisterState cid wasm_state =
  modify (\ic -> ic { canisters =
    M.adjust (fmap (\cs -> cs { wasm_state })) cid (canisters ic)
  })

dummyCanisterId :: CanisterId
dummyCanisterId = BS.pack [0xDE, 0xAD, 0xBE, 0xEF]

dummyUserId :: CanisterId
dummyUserId = BS.pack [0xCA, 0xFF, 0xEE]

processRequest :: ICT m => AsyncRequest -> m ()
processRequest r@(InstallRequest can_mod arg) =
  case parseCanister can_mod of
    Left err ->
      setReqStatus r (Rejected (RC_SYS_FATAL, "Parsing failed: " ++ err))
    Right can_mod ->
      case init_method can_mod (dummyCanisterId, arg) of
        Trap msg ->
          setReqStatus r (Rejected (RC_CANISTER_ERROR, "Initialization trapped: " ++ msg))
        Return wasm_state -> do
          createCanister dummyCanisterId can_mod wasm_state
          setReqStatus r (Completed CompleteUnit)

processRequest r@(UpdateRequest method arg) = do
  setReqStatus r Processing
  gets (M.lookup dummyCanisterId . canisters) >>= \case
    Nothing -> setReqStatus r $ Rejected (RC_DESTINATION_INVALID, "canister does not exist")
    Just Nothing -> setReqStatus r $ Rejected (RC_DESTINATION_INVALID, "canister is empty")
    Just (Just (CanState wasm_state can_mod)) ->
      case M.lookup method (update_methods can_mod) of
        Nothing -> setReqStatus r $ Rejected (RC_DESTINATION_INVALID, "update method does not exist")
        Just f ->
          case f (Arg arg dummyUserId) wasm_state of
            Trap msg -> setReqStatus r $ Rejected (RC_CANISTER_ERROR, "canister trapped: " ++ msg)
            Return (new_state, mb_response) -> do
              setCanisterState dummyCanisterId new_state
              case mb_response of
                Just (Reply res) -> setReqStatus r $ Completed (CompleteArg res)
                Just (Reject (rc,rm)) -> setReqStatus r $ Rejected (rc, rm)
                Nothing -> return ()

-- processRequest r = setReqStatus r (Rejected (0, "Unknown request type"))

starvedRequest :: ICT m => AsyncRequest -> m ()
starvedRequest r@(UpdateRequest method arg) =
  setReqStatus r $ Rejected (RC_CANISTER_ERROR, "canister did not respond")

runToCompletion :: ICT m => m ()
runToCompletion =
  nextRecieved >>= \case
    Just  r -> processRequest r >> runToCompletion
    Nothing -> nextStarved >>= \case
      Just  r -> starvedRequest r >> runToCompletion
      Nothing -> return ()
