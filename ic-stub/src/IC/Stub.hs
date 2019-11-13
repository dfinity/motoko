{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
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

import IC.Wasm.Winter

type (↦) = M.Map

-- Basic types

type Blob = BS.ByteString
type EntityId = Blob
type CanisterId = EntityId
type MethodName = String
type RequestID = Blob

-- Reject code

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

data TrapOr a = Trap String | Return a

data Arg = Arg
  { dat :: Blob
  , caller :: EntityId
  }
  deriving Show

type InitFunc = (CanisterId, Blob) -> TrapOr WasmState
type UpdateFunc = WasmState -> TrapOr (WasmState, {- List MethodCall, -} Maybe Response)
type QueryFunc = WasmState -> TrapOr Response
data Response = Reply Blob | Reject (RejectCode, String)

data CanisterModule = CanisterModule
  { init_method :: InitFunc
  , update_methods :: MethodName ↦ (Arg -> UpdateFunc)
  , query_methods :: MethodName ↦ (Arg -> QueryFunc)
  }

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

-- Concrete canister

-- We model a WasmState as the history of the canister. This is slow,
-- but allows us a correct implementation of persistence based on any
-- WebAssembly engine.

data WasmState
  = WSInit Module CanisterId Blob
  | WSUpdate MethodName Arg WasmState

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

data Params = Params
  { param_dat  :: Maybe Blob
  , param_caller :: Maybe EntityId
  , reject_code :: Int
  , reject_message :: String
  }

data ExecutionState s = ExecutionState
  { inst :: Instance s
  , self_id :: CanisterId
  , params :: Params
  , response :: Maybe Response
  , reply_data :: Blob
  }



initalExecutionState :: CanisterId -> Instance s -> ExecutionState s
initalExecutionState self_id inst = ExecutionState
  { inst
  , self_id
  , params = Params Nothing Nothing 0 ""
  , response = Nothing
  , reply_data = mempty
  }

type ESRef s = (STRef s Bool, STRef s (Maybe (ExecutionState s)))

newESRef :: ST s (ESRef s)
newESRef = (,) <$> newSTRef True <*> newSTRef Nothing

runESST :: (forall s. ESRef s -> ST s a) -> a
runESST f = runST $ newESRef >>= f

withES :: PrimMonad m => ESRef (PrimState m) -> ExecutionState (PrimState m) -> m a -> m (a, ExecutionState (PrimState m))
withES (pref, esref) es f = do
  before <- stToPrim $ readSTRef esref
  unless (isNothing before) $ fail "withES with non-empty es"
  stToPrim $ writeSTRef esref $ Just es
  x <- f
  es' <- stToPrim $ readSTRef esref
  case es' of
    Nothing -> fail "withES: ExecutionState lost"
    Just es' -> do
      stToPrim $ writeSTRef esref Nothing
      return (x, es')

silently :: PrimMonad m => ESRef (PrimState m) -> m x -> m x
silently (pref, esref) f = do
  before <- stToPrim $ readSTRef pref
  stToPrim $ writeSTRef pref False
  x <- f
  stToPrim $ writeSTRef pref before
  return x

getsES :: ESRef s -> (ExecutionState s -> b) -> ExceptT String (ST s) b
getsES (_, esref) f = lift (readSTRef esref) >>= \case
      Nothing -> throwError "System API not available yet"
      Just es -> return (f es)

modES :: ESRef s -> (ExecutionState s -> ExecutionState s) -> ExceptT String (ST s) ()
modES (_, esref) f = lift $ modifySTRef esref (fmap f)


putBytes :: PrimMonad m => ESRef (PrimState m) -> BS.ByteString -> m ()
putBytes (pref, _esref) bytes =
  stToPrim (readSTRef pref) >>= \case
    True -> unsafeIOToPrim (BSC.putStrLn bytes)
    False -> return ()


systemAPI :: forall s. ESRef s -> Imports s
systemAPI esref =
    [ (,) "ic"
      [ (,,,) "trap" [i32, i32] [] $ \case
          [v1,v2] -> throwError "explicit trap"
          _ -> fail "ic.trap: invalid argument"
      ]
    , (,) "msg"
      [ (,,,) "reject" [i64, i32, i32] [] msg_reject
      , (,,,) "reply" [i64, i32, i32] [] msg_reply
      , unimplemented "error_code" [i64] [i32]
      , (,,,) "arg_data_copy" [i64, i32, i32, i32] [] arg_data_copy
      , (,,,) "arg_data_size" [i64] [i32] arg_data_size
      ]
    , (,) "debug"
      [ (,,,) "print" [i32, i32] [] debug_print
      ]
    ]
  where
    unimplemented name arg ret = (,,,) name arg ret $
      \_ -> throwError $ "unimplemented: " ++ name

    i32 = I32Type
    i64 = I64Type

    debug_print :: [Value] -> HostFunc s
    debug_print [I32 ptr, I32 len] = do
      i <- getsES esref inst
      bytes <- getBytes i (fromIntegral ptr) len
      putBytes esref bytes
      return []
    debug_print _ = fail "debug_print: invalid argument"

    arg_data_size :: [Value] -> HostFunc s
    arg_data_size [I64 _nonce] = do
      blob <- getsES esref (param_dat . params)
          >>= maybe (throwError "arg_data_size: No argument") return
      return [I32 (fromIntegral (BS.length blob))]
    arg_data_size _ = fail "arg_data_size: invalid argument"

    arg_data_copy :: [Value] -> HostFunc s
    arg_data_copy [I64 _nonce, I32 dst, I32 len, I32 offset] = do
      blob <- getsES esref (param_dat . params)
          >>= maybe (throwError "arg_data_size: No argument") return
      unless (offset == 0) $ throwError "arg_data_copy: offset /= 0 not suppoted"
      unless (len == fromIntegral (BS.length blob)) $ throwError "arg_data_copy: len not full blob not suppoted"
      i <- getsES esref inst
      -- TODO Bounds checking
      setBytes i (fromIntegral dst) blob
      return []
    arg_data_copy _ = fail "arg_data_copy: invalid argument"

    msg_reply :: [Value] -> HostFunc s
    msg_reply [I64 _nonce, I32 ptr, I32 len] = do
      i <- getsES esref inst
      bytes <- getBytes i (fromIntegral ptr) len
      modES esref (\es -> es { response = Just (Reply bytes) })
      return []
    msg_reply _ = fail "msg_reply: invalid argument"

    msg_reject :: [Value] -> HostFunc s
    msg_reject [I64 _nonce, I32 ptr, I32 len] = do
      i <- getsES esref inst
      bytes <- getBytes i (fromIntegral ptr) len
      let msg = BSU.toString bytes
      modES esref (\es -> es { response = Just (Reject (RC_CANISTER_REJECT, msg)) })
      return []
    msg_reject _ = fail "msg_reply: invalid argument"



ifStateIsPresent :: Monad m => m x -> StateT s m x -> StateT (Maybe s) m x
ifStateIsPresent err (StateT f) = StateT $ \case
  Nothing -> (, Nothing) <$> err
  Just es -> do
    (r, es') <- f es
    return (r, Just es')

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

type RawState s = (ESRef s, CanisterId, Instance s)

rawInitializeMethod :: ESRef s -> Module -> CanisterId -> Blob -> ST s (TrapOr (RawState s))
rawInitializeMethod esref wasm_mod cid arg = do
  result <- runExceptT $ do
    inst <- initialize wasm_mod (systemAPI esref)

    --  invoke canister_init
    when ("canister_init" `elem` exportedFunctions wasm_mod) $
      void $ withES esref (initalExecutionState cid inst) $
         invokeExport inst "canister_init" [I64 0]

    return (esref, cid, inst)
  case result of
    Left  err     -> return $ Trap err
    Right raw_state -> return $ Return raw_state

rawQueryMethod :: RawState s -> MethodName -> Arg -> ST s (TrapOr Response)
rawQueryMethod (esref, cid, inst) method arg = do
  let es = (initalExecutionState cid inst)
            { params = Params
                { param_dat    = Just $ dat arg
                , param_caller = Just $ caller arg
                , reject_code  = 0
                , reject_message = ""
                }
            }
  result <- runExceptT $ withES esref es $
    invokeExport inst ("canister_query " ++ method) [I64 0]
  case result of
    Left err -> return $ Trap err
    Right (_, es')
      | Just r <- response es' -> return $ Return r
      | otherwise -> return $ Trap "No response"

rawUpdateMethod :: RawState s -> MethodName -> Arg -> ST s (TrapOr ({- List MethodCall, -} Maybe Response))
rawUpdateMethod (esref, cid, inst) method arg = do
  let es = (initalExecutionState cid inst)
            { params = Params
                { param_dat    = Just $ dat arg
                , param_caller = Just $ caller arg
                , reject_code  = 0
                , reject_message = ""
                }
            }

  result <- runExceptT $ withES esref es $
    invokeExport inst ("canister_update " ++ method) [I64 0]
  case result of
    Left  err -> return $ Trap err
    Right (_, es') -> return $ Return (response es')
