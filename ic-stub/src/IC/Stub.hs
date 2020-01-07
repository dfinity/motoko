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

import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Monad.State.Class
import Data.Sequence (Seq(..))
import Data.Foldable (toList)

-- import Debug.Trace

import IC.Types
import IC.Canister
import IC.Id
import IC.Logger

-- Abstract HTTP Interface

data AsyncRequest
    = CreateRequest UserId (Maybe CanisterId)
    | InstallRequest CanisterId UserId Blob Blob
    | UpdateRequest CanisterId UserId MethodName Blob
  deriving (Eq, Ord, Show)

data SyncRequest
    = QueryRequest CanisterId UserId MethodName Blob
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

type ICT m = (MonadState IC m, Logger m)

data EntryPoint
  = Public MethodName Blob
  | Closure Callback Response
  deriving Show

type CallId = Int

data CallContext = CallContext
  { canister :: CanisterId
  , origin :: CallOrigin
  , responded :: Responded
  , last_trap :: Maybe String
  }
  deriving Show

data CallOrigin
  = FromUser AsyncRequest
  | FromCanister CallId Callback
  | FromInit EntityId
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
  , messages :: Seq Message
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

readRequest (QueryRequest canister_id user_id method arg) =
  gets (M.lookup canister_id . canisters) >>= \case
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
  [ c
  | (c, ctxt) <- M.toList (call_contexts ic)
  , Responded False <- return $ responded ctxt
  , null [ () | ResponseMessage { call_context = c' } <- toList (messages ic), c' == c ]
  , null
      [ ()
      | CallContext { responded = Responded False, origin = FromCanister c' _}
          <- M.elems (call_contexts ic)
      , c' == c
      ]
  ]

nextMessage :: ICT m => m (Maybe Message)
nextMessage = state $ \ic ->
  case messages ic of
    Empty -> (Nothing, ic)
    m :<| ms -> (Just m, ic { messages = ms })

setReqStatus :: ICT m => AsyncRequest -> RequestStatus -> m ()
setReqStatus r s =
  modify (\ic -> ic { requests = M.insert r s (requests ic) })

createEmptyCanister :: ICT m => CanisterId -> m ()
createEmptyCanister cid =
  modify (\ic -> ic { canisters =
    M.insert cid Nothing (canisters ic)
  })


installCanister :: ICT m => CanisterId -> CanisterModule -> WasmState -> m ()
installCanister cid can_mod wasm_state =
  -- Check that canister exists but is empty before?
  modify (\ic -> ic { canisters =
    M.insert cid (Just (CanState {can_mod, wasm_state})) (canisters ic)
  })

setCanisterState :: ICT m => CanisterId -> WasmState -> m ()
setCanisterState cid wasm_state =
  modify (\ic -> ic { canisters =
    M.adjust (fmap (\cs -> cs { wasm_state })) cid (canisters ic)
  })

processRequest :: ICT m => AsyncRequest -> m ()

processRequest r@(CreateRequest _user_id (Just desired)) = do
  exists <- gets (M.member desired . canisters)
  if exists
  then
    setReqStatus r $ Rejected (RC_DESTINATION_INVALID, "Desired canister id already exists")
  else do
    createEmptyCanister desired
    setReqStatus r $ Completed $ CompleteCanisterId desired

processRequest r@(CreateRequest _user_id Nothing) = do
  existing_canisters <- gets (M.keys . canisters)
  let new_id = freshId existing_canisters
  createEmptyCanister new_id
  setReqStatus r $ Completed $ CompleteCanisterId new_id

processRequest r@(InstallRequest canister_id user_id can_mod dat) =
  case parseCanister can_mod of
    Left err ->
      setReqStatus r $ Rejected (RC_SYS_FATAL, "Parsing failed: " ++ err)
    Right can_mod -> do
      -- We only need a call context to be able to do inter-canister calls
      -- from init, which is useful for Motoko testing, but not currently
      -- allowed by the spec.
      ctxt_id <- newCallContext $ CallContext
        { canister = canister_id
        , origin = FromInit user_id
        , responded = Responded True
        , last_trap = Nothing
        }

      existing_canisters <- gets (M.keys . canisters)

      case init_method can_mod existing_canisters canister_id user_id dat of
        Trap msg ->
          setReqStatus r $ Rejected (RC_CANISTER_ERROR, "Initialization trapped: " ++ msg)
        Return ((new_canisters, new_calls), wasm_state) -> do
          installCanister canister_id can_mod wasm_state
          mapM_ (\(i,_,_) -> createEmptyCanister i) new_canisters
          mapM_ (\(i,mod,dat) -> submitRequest (InstallRequest i canister_id mod dat)) new_canisters
          mapM_ (newCall ctxt_id) new_calls
          setReqStatus r $ Completed CompleteUnit

processRequest r@(UpdateRequest canister_id _user_id method arg) = do
  ctxt_id <- newCallContext $ CallContext
    { canister = canister_id
    , origin = FromUser r
    , responded = Responded False
    , last_trap = Nothing
    }
  enqueueMessage $ CallMessage
    { call_context = ctxt_id
    , entry = Public method arg
    }
  setReqStatus r Processing

enqueueMessage :: ICT m => Message -> m ()
enqueueMessage m = modify $ \ic -> ic { messages = messages ic :|> m }

newCallContext :: ICT m => CallContext -> m CallId
newCallContext cc = state go
  where
    go ic  = (i, ic { call_contexts = M.insert i cc (call_contexts ic)})
      where
        i | M.null (call_contexts ic) = 0
          | otherwise = fst (M.findMax (call_contexts ic)) + 1

getCallContext :: ICT m => CallId -> m CallContext
getCallContext ctxt_id =
  gets ((M.! ctxt_id) . call_contexts)

modifyCallContext :: ICT m => CallId -> (CallContext -> CallContext) -> m ()
modifyCallContext ctxt_id f =
  modify $ \ic -> ic { call_contexts = M.adjust f ctxt_id (call_contexts ic) }

respondCallContext :: ICT m => CallId -> Response -> m ()
respondCallContext ctxt_id response = do
  -- TODO: check no prior response
  modifyCallContext ctxt_id $ \ctxt -> ctxt { responded = Responded True }
  enqueueMessage $ ResponseMessage { call_context = ctxt_id, response }

rememberTrap :: ICT m => CallId -> String -> m ()
rememberTrap ctxt_id msg =
  modifyCallContext ctxt_id $ \ctxt -> ctxt { last_trap = Just msg }

callerOfRequest :: AsyncRequest -> EntityId
callerOfRequest (CreateRequest user_id _) = user_id
callerOfRequest (InstallRequest _ user_id _ _) = user_id
callerOfRequest (UpdateRequest _ user_id _ _) = user_id

callerOfCallID :: ICT m => CallId -> m EntityId
callerOfCallID ctxt_id = do
  ctxt <- getCallContext ctxt_id
  case origin ctxt of
    FromUser request -> return $ callerOfRequest request
    FromCanister other_ctxt_id _callback -> calleeOfCallID other_ctxt_id
    FromInit entity_id -> return entity_id

calleeOfCallID :: ICT m => CallId -> m EntityId
calleeOfCallID ctxt_id = canister <$> getCallContext ctxt_id

respondedCallID :: ICT m => CallId -> m Responded
respondedCallID ctxt_id = responded <$> getCallContext ctxt_id

invokeEntry :: ICT m =>
    CallId -> CanState -> EntryPoint ->
    m (TrapOr (WasmState, UpdateResult))
invokeEntry ctxt_id (CanState wasm_state can_mod) entry = do
    responded <- respondedCallID ctxt_id
    existing_canisters <- gets (M.keys . canisters)
    case entry of
      Public method dat -> do
        caller <- callerOfCallID ctxt_id
        case M.lookup method (update_methods can_mod) of
          Just f ->
            return $ f existing_canisters caller responded dat wasm_state
          Nothing -> do
            let reject = Reject (RC_DESTINATION_INVALID, "update method does not exist: " ++ method)
            return $ Return (wasm_state, ([], [], Just reject))
      Closure cb r ->
        return $ callbacks can_mod cb existing_canisters responded r wasm_state

newCall :: ICT m => CallId -> MethodCall -> m ()
newCall from_ctxt_id call = do
  new_ctxt_id <- newCallContext $ CallContext
    { canister = call_callee call
    , origin = FromCanister from_ctxt_id (call_callback call)
    , responded = Responded False
    , last_trap = Nothing
    }
  enqueueMessage $ CallMessage
    { call_context = new_ctxt_id
    , entry = Public (call_method_name call) (call_arg call)
    }

processMessage :: ICT m => Message -> m ()
processMessage (CallMessage ctxt_id entry) = do
  callee <- calleeOfCallID ctxt_id
  let res r = respondCallContext ctxt_id r
  gets (M.lookup callee . canisters) >>= \case
    Nothing -> res $ Reject (RC_DESTINATION_INVALID, "canister does not exist: " ++ show callee)
    Just Nothing -> res $ Reject (RC_DESTINATION_INVALID, "canister is empty")
    Just (Just cs) ->
      invokeEntry ctxt_id cs entry >>= \case
        Trap msg -> do
          logTrap msg
          rememberTrap ctxt_id msg
        Return (new_state, (new_canisters, new_calls, mb_response)) -> do
          setCanisterState callee new_state
          mapM_ (\(i,_,_) -> createEmptyCanister i) new_canisters
          mapM_ (\(i,mod,dat) -> submitRequest (InstallRequest i callee mod dat)) new_canisters
          mapM_ (newCall ctxt_id) new_calls
          mapM_ res mb_response

processMessage (ResponseMessage ctxt_id response) = do
  ctxt <- getCallContext ctxt_id
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
    FromInit _ -> fail "unexpected Response in Init"

starveCallContext :: ICT m => CallId -> m ()
starveCallContext ctxt_id = do
  ctxt <- getCallContext ctxt_id
  let msg | Just t <- last_trap ctxt = "canister trapped: " ++ t
          | otherwise                = "canister did not respond"
  respondCallContext ctxt_id $ Reject (RC_CANISTER_ERROR, msg)

runToCompletion :: ICT m => m ()
runToCompletion =
  nextReceived >>= \case
    Just  r -> processRequest r >> runToCompletion
    Nothing -> nextMessage >>= \case
      Just m  -> processMessage m >> runToCompletion
      Nothing -> nextStarved >>= \case
        Just c  -> starveCallContext c >> runToCompletion
        Nothing -> return ()
