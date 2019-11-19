{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wmissing-signatures #-}
module IC.Stub
  ( IC
  , AsyncRequest(..)
  , SyncRequest(..)
  , RequestStatus(..)
  , CompletionValue(..)
  , ReqResponse
  , initialIC
  , submitRequest, readRequest
  , runToCompletion
  , requestId
  )
where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State.Class

-- import Debug.Trace

import IC.Types
import IC.Canister

-- Abstract HTTP Interface

data AsyncRequest
    = InstallRequest UserId Blob Blob
    | UpdateRequest UserId MethodName Blob
  deriving (Eq, Ord, Show)

data SyncRequest
    = QueryRequest UserId MethodName Blob
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

data EntryPoint
  = Public MethodName Blob
  | Closure Callback Response
  deriving Show

type CallId = Int

data CallContext = CallContext
  { canister :: CanisterId
  , origin :: CallOrigin
  , responded :: Bool
  , last_trap :: Maybe String
  }
  deriving Show

data CallOrigin
  = FromUser AsyncRequest
  | FromCanister CallId Callback
  deriving Show

data Message =
  CallMessage
    { call_context :: CallId
    , entry :: EntryPoint
    }
  | ResponseMessage
    { call_context :: CallId
    , response :: Response
    }
  deriving Show

data IC = IC
  { canisters :: CanisterId ↦ Maybe CanState
  , requests :: AsyncRequest ↦ RequestStatus
  , messages :: [Message]
  , call_contexts :: CallId ↦ CallContext
  }

initialIC :: IC
initialIC = IC mempty mempty mempty mempty

submitRequest :: ICT m => AsyncRequest -> m ()
submitRequest r =
  -- lensify?
  modify (\ic -> ic { requests = M.insert r Received (requests ic) })

requestId :: AsyncRequest -> Blob
requestId r = BSC.pack (show r) -- TODO: Implement request hashing

findRequest :: RequestID -> IC -> Maybe (AsyncRequest, RequestStatus)
findRequest rid ic = find (\(r,_s) -> requestId r == rid) (M.toList (requests ic))

readRequest :: ICT m => SyncRequest -> m ReqResponse

readRequest (StatusRequest rid) =
  gets (findRequest rid) >>= \case
    Just (_r,status) -> return status
    Nothing -> return Unknown

readRequest (QueryRequest user_id method arg) =
  gets (M.lookup dummyCanisterId . canisters) >>= \case
    Nothing -> return $ Rejected (RC_DESTINATION_INVALID, "canister does not exist")
    Just Nothing -> return $ Rejected (RC_DESTINATION_INVALID, "canister is empty")
    Just (Just (CanState wasm_state can_mod)) ->
      case M.lookup method (query_methods can_mod) of
        Nothing -> return $ Rejected (RC_DESTINATION_INVALID, "query method does not exist")
        Just f ->
          case f user_id arg wasm_state of
            Trap msg -> return $ Rejected (RC_CANISTER_ERROR, "canister trapped: " ++ msg)
            Return (Reply res) -> return $ Completed (CompleteArg res)
            Return (Reject (rc,rm)) -> return $ Rejected (rc, rm)

nextReceived :: ICT m => m (Maybe AsyncRequest)
nextReceived = gets $ \ic -> listToMaybe
  [ r | (r, Received) <- M.toList (requests ic) ]

nextStarved :: ICT m => m (Maybe CallId)
nextStarved = gets $ \ic -> listToMaybe
  -- TODO: This should return a starved calling context.
  -- Until we have them, we treat requests in Processing state as starved
  [ c
  | (c, ctxt) <- M.toList (call_contexts ic)
  , not $ responded ctxt
  , null [ () | ResponseMessage { call_context = c' } <- messages ic, c' == c ]
  , null
      [ ()
      | CallContext { responded = False, origin = FromCanister c' _}
          <- M.elems (call_contexts ic)
      , c' == c
      ]
  ]

nextMessage :: ICT m => m (Maybe Message)
nextMessage = state $ \ic ->
  case messages ic of
    [] -> (Nothing, ic)
    (m:ms) -> (Just m, ic { messages = ms })

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
dummyCanisterId = EntityId $ BS.pack [0xDE, 0xAD, 0xBE, 0xEF]

processRequest :: ICT m => AsyncRequest -> m ()
processRequest r@(InstallRequest user_id can_mod dat) = do
  let canister_id = dummyCanisterId
  case parseCanister can_mod of
    Left err ->
      setReqStatus r $ Rejected (RC_SYS_FATAL, "Parsing failed: " ++ err)
    Right can_mod ->
      case init_method can_mod canister_id user_id dat of
        Trap msg ->
          setReqStatus r $ Rejected (RC_CANISTER_ERROR, "Initialization trapped: " ++ msg)
        Return wasm_state -> do
          createCanister canister_id can_mod wasm_state
          setReqStatus r $ Completed CompleteUnit

processRequest r@(UpdateRequest _user_id method arg) = do
  let canister_id = dummyCanisterId

  ctxt_id <- newCallContext $ CallContext
    { canister = canister_id
    , origin = FromUser r
    , responded = False
    , last_trap = Nothing
    }
  enqueueMessage $ CallMessage
    { call_context = ctxt_id
    , entry = Public method arg
    }
  setReqStatus r Processing

-- processRequest r = setReqStatus r (Rejected (0, "Unknown request type"))

enqueueMessage :: ICT m => Message -> m ()
enqueueMessage m = modify $ \ic -> ic { messages = messages ic ++ [m] }

newCallContext :: ICT m => CallContext -> m CallId
newCallContext cc = state go
  where
    go ic  = (i, ic { call_contexts = M.insert i cc (call_contexts ic)})
      where
        i | M.null (call_contexts ic) = 0
          | otherwise = fst (M.findMax (call_contexts ic)) + 1

modifyContext :: ICT m => CallId -> (CallContext -> CallContext) -> m ()
modifyContext ctxt_id f =
  modify $ \ic -> ic { call_contexts = M.adjust f ctxt_id (call_contexts ic) }

respondContext :: ICT m => CallId -> Response -> m ()
respondContext ctxt_id response = do
  -- TODO: check no prior response
  modifyContext ctxt_id $ \ctxt -> ctxt { responded = True }
  enqueueMessage $ ResponseMessage { call_context = ctxt_id, response }

rememberTrap :: ICT m => CallId -> String -> m ()
rememberTrap ctxt_id msg =
  modifyContext ctxt_id $ \ctxt -> ctxt { last_trap = Just msg }

callerOfRequest :: AsyncRequest -> EntityId
callerOfRequest (InstallRequest user_id _ _) = user_id
callerOfRequest (UpdateRequest user_id _ _) = user_id

callerOfCallID :: ICT m => CallId -> m EntityId
callerOfCallID ctxt_id = do
  ctxt <- gets ((M.! ctxt_id) . call_contexts)
  case origin ctxt of
    FromUser request -> return $ callerOfRequest request
    FromCanister other_ctxt_id _callback -> calleeOfCallID other_ctxt_id

calleeOfCallID :: ICT m => CallId -> m EntityId
calleeOfCallID ctxt_id = do
  ctxt <- gets ((M.! ctxt_id) . call_contexts)
  return $ canister ctxt

invokeEntry :: ICT m =>
    CallId -> CanState -> EntryPoint ->
    m (TrapOr (WasmState, ([MethodCall], Maybe Response)))
invokeEntry ctxt_id (CanState wasm_state can_mod) entry = do
    caller <- callerOfCallID ctxt_id
    case entry of
      Public method dat ->
        case M.lookup method (update_methods can_mod) of
          Just f -> return $ f caller dat wasm_state
          Nothing -> return $ Return (wasm_state, ([], Just $ Reject (RC_DESTINATION_INVALID, "update method does not exist: " ++ method)))
      Closure cb r ->
        return $ callbacks can_mod cb caller r wasm_state

processMessage :: ICT m => Message -> m ()
processMessage (CallMessage ctxt_id entry) = do
  callee <- calleeOfCallID ctxt_id
  let res r = respondContext ctxt_id r
  gets (M.lookup callee . canisters) >>= \case
    Nothing -> res $ Reject (RC_DESTINATION_INVALID, "canister does not exist: " ++ show callee)
    Just Nothing -> res $ Reject (RC_DESTINATION_INVALID, "canister is empty")
    Just (Just cs) ->
      invokeEntry ctxt_id cs entry >>= \case
        Trap msg ->
          rememberTrap ctxt_id msg
        Return (new_state, (calls, mb_response)) -> do
          setCanisterState callee new_state
          forM_ calls $ \call -> do
            new_ctxt_id <- newCallContext $ CallContext
              { canister = call_callee call
              , origin = FromCanister ctxt_id (call_callback call)
              , responded = False
              , last_trap = Nothing
              }
            enqueueMessage $ CallMessage
              { call_context = new_ctxt_id
              , entry = Public (call_method_name call) (call_arg call)
              }
          mapM_ res mb_response

processMessage (ResponseMessage ctxt_id response) = do
  ctxt <- gets ((M.! ctxt_id) . call_contexts)
  case origin ctxt of
    FromUser request -> setReqStatus request $
      case response of
        Reject (rc, msg) -> Rejected (rc, msg)
        Reply blob -> Completed (CompleteArg blob)
    FromCanister other_ctxt_id callback ->
      enqueueMessage $ CallMessage
        { call_context = other_ctxt_id
        , entry = Closure callback response
        }

starveContext :: ICT m => CallId -> m ()
starveContext ctxt_id = do
  ctxt <- gets ((M.! ctxt_id) . call_contexts)
  let msg | Just t <- last_trap ctxt = "canister trapped: " ++ t
          | otherwise                = "canister did not respond"
  respondContext ctxt_id $ Reject (RC_CANISTER_ERROR, msg)

runToCompletion :: ICT m => m ()
runToCompletion =
  nextReceived >>= \case
    Just  r -> processRequest r >> runToCompletion
    Nothing -> nextMessage >>= \case
      Just m  -> processMessage m >> runToCompletion
      Nothing -> nextStarved >>= \case
        Just c  -> starveContext c >> runToCompletion
        Nothing -> return ()
