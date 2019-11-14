{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
The canister interface, presented imperatively (or impurely), i.e. without rollback
-}
module IC.Canister.Imp
 ( ESRef
 , RawState
 , runESST
 , rawInitializeMethod
 , rawUpdateMethod
 , rawCallbackMethod
 , rawQueryMethod
 , silently
 )
where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSU
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Except
import Data.STRef
import Data.Maybe
import Data.Int

import IC.Types
import IC.Wasm.Winter

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
  , calls :: [MethodCall]
  }


initalExecutionState :: CanisterId -> Instance s -> ExecutionState s
initalExecutionState self_id inst = ExecutionState
  { inst
  , self_id
  , params = Params Nothing Nothing 0 ""
  , response = Nothing
  , calls = mempty
  }

type ESRef s = (STRef s Bool, STRef s (Maybe (ExecutionState s)))

newESRef :: ST s (ESRef s)
newESRef = (,) <$> newSTRef True <*> newSTRef Nothing

runESST :: (forall s. ESRef s -> ST s a) -> a
runESST f = runST $ newESRef >>= f

withES :: PrimMonad m => ESRef (PrimState m) -> ExecutionState (PrimState m) -> m a -> m (a, ExecutionState (PrimState m))
withES (_pref, esref) es f = do
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
silently (pref, _esref) f = do
  before <- stToPrim $ readSTRef pref
  stToPrim $ writeSTRef pref False
  x <- f
  stToPrim $ writeSTRef pref before
  return x

getsES :: ESRef s -> (ExecutionState s -> b) -> HostM s b
getsES (_, esref) f = lift (readSTRef esref) >>= \case
      Nothing -> throwError "System API not available yet"
      Just es -> return (f es)

modES :: ESRef s -> (ExecutionState s -> ExecutionState s) -> HostM s ()
modES (_, esref) f = lift $ modifySTRef esref (fmap f)

setResponse :: ESRef s -> Response -> HostM s ()
setResponse esref r = modES esref $ \es ->
  es { response = Just r }

appendCall :: ESRef s -> MethodCall -> HostM s ()
appendCall esref c = modES esref $ \es ->
  es { calls = calls es ++ [c] }

putBytes :: PrimMonad m => ESRef (PrimState m) -> BS.ByteString -> m ()
putBytes (pref, _esref) bytes =
  stToPrim (readSTRef pref) >>= \case
    True -> unsafeIOToPrim (BSC.putStrLn bytes)
    False -> return ()


systemAPI :: forall s. ESRef s -> Imports s
systemAPI esref =
    [ (,) "ic0"
      [ (,,,) "call_simple" (replicate 10 i32) [i32] call_simple
      , (,,,) "canister_self_copy" (replicate 3 i32) [] canister_self_copy
      , (,,,) "canister_self_len" [] [i32] canister_self_len
      , (,,,) "debug_print" [i32, i32] [] debug_print
      , (,,,) "msg_arg_data_copy" [i32, i32, i32] [] arg_data_copy
      , (,,,) "msg_arg_data_size" [] [i32] arg_data_size
      , (,,,) "msg_reject" [i32, i32] [] msg_reject
      , (,,,) "msg_reply" [i32, i32] [] msg_reply
      , (,,,) "trap" [i32, i32] [] explicit_trap
      , unimplemented "msg_error_code" [] [i32]
      ]
    ]
  where
    -- Utilities

    unimplemented name arg ret = (,,,) name arg ret $
      \_ -> throwError $ "unimplemented: " ++ name

    i32 = I32Type

    copy_to_canister :: String -> Int32 -> Int32 -> Int32 -> Blob -> HostM s ()
    copy_to_canister name dst offset len blob = do
      unless (offset == 0) $
        throwError $ name ++ ": offset /= 0 not suppoted"
      unless (len == fromIntegral (BS.length blob)) $
        throwError $ name ++ ": len not full blob not suppoted"
      i <- getsES esref inst
      -- TODO Bounds checking
      setBytes i (fromIntegral dst) blob

    copy_from_canister :: String -> Int32 -> Int32 -> HostM s Blob
    copy_from_canister _name src len = do
      i <- getsES esref inst
      getBytes i (fromIntegral src) len

    -- The system calls

    -- (TODO: Give them proper Haskell types and write generic code that takes
    -- apart the values)

    debug_print :: [Value] -> HostFunc s
    debug_print [I32 src, I32 len] = do
      -- TODO: This should be a non-trapping copy
      bytes <- copy_from_canister "debug_print" src len
      putBytes esref bytes
      return []
    debug_print _ = fail "debug_print: invalid argument"

    explicit_trap :: [Value] -> HostFunc s
    explicit_trap [I32 src, I32 len] = do
      -- TODO: This should be a non-trapping copy
      bytes <- copy_from_canister "ic.trap" src len
      let msg = BSU.toString bytes
      throwError $ "canister trapped explicitly: " ++ msg
    explicit_trap _ = fail "explicit_trap: invalid argument"

    canister_self_len :: [Value] -> HostFunc s
    canister_self_len [] = do
      id <- getsES esref self_id
      return [I32 (fromIntegral (BS.length (rawEntityId id)))]
    canister_self_len _ = fail "arg_data_size: invalid argument"

    canister_self_copy :: [Value] -> HostFunc s
    canister_self_copy [I32 dst, I32 offset, I32 len] = do
      id <- getsES esref self_id
      copy_to_canister "canister_self_copy" dst offset len (rawEntityId id)
      return []
    canister_self_copy _ = fail "arg_data_size: invalid argument"

    arg_data_size :: [Value] -> HostFunc s
    arg_data_size [] = do
      blob <- getsES esref (param_dat . params)
          >>= maybe (throwError "arg_data_size: No argument") return
      return [I32 (fromIntegral (BS.length blob))]
    arg_data_size _ = fail "arg_data_size: invalid argument"

    arg_data_copy :: [Value] -> HostFunc s
    arg_data_copy [I32 dst, I32 offset, I32 len] = do
      blob <- getsES esref (param_dat . params)
          >>= maybe (throwError "arg_data_size: No argument") return
      copy_to_canister "arg_data_copy" dst offset len blob
      return []
    arg_data_copy _ = fail "arg_data_copy: invalid argument"

    msg_reply :: [Value] -> HostFunc s
    msg_reply [I32 src, I32 len] = do
      bytes <- copy_from_canister "debug_print" src len
      setResponse esref (Reply bytes)
      return []
    msg_reply _ = fail "msg_reply: invalid argument"

    msg_reject :: [Value] -> HostFunc s
    msg_reject [I32 src, I32 len] = do
      bytes <- copy_from_canister "debug_print" src len
      let msg = BSU.toString bytes
      modES esref (\es -> es { response = Just (Reject (RC_CANISTER_REJECT, msg)) })
      return []
    msg_reject _ = fail "msg_reply: invalid argument"

    call_simple :: [Value] -> HostFunc s
    call_simple
      [ I32 callee_src
      , I32 callee_len
      , I32 name_src
      , I32 name_len
      , I32 reply_fun
      , I32 reply_env
      , I32 reject_fun
      , I32 reject_env
      , I32 data_src
      , I32 data_len
      ] = do
      callee <- copy_from_canister "call_simple" callee_src callee_len
      method_name <- copy_from_canister "call_simple" name_src name_len
      arg <- copy_from_canister "call_simple" data_src data_len

      appendCall esref $ MethodCall
        { call_callee = EntityId callee
        , call_method_name = BSU.toString method_name -- TODO: check for valid UTF8
        , call_arg = arg
        , call_callback = Callback
            { reply_callback = WasmClosure reply_fun reply_env
            , reject_callback = WasmClosure reject_fun reject_env
            }
        }

      return [I32 0]
    call_simple _ = fail "call_simple: invalid argument"

type RawState s = (ESRef s, CanisterId, Instance s)

rawInitializeMethod :: ESRef s -> Module -> CanisterId -> EntityId -> Blob -> ST s (TrapOr (RawState s))
rawInitializeMethod esref wasm_mod cid caller dat = do
  result <- runExceptT $ do
    inst <- initialize wasm_mod (systemAPI esref)

    let es = (initalExecutionState cid inst)
              { params = Params
                  { param_dat    = Just dat
                  , param_caller = Just caller
                  , reject_code  = 0
                  , reject_message = ""
                  }
              }

    --  invoke canister_init
    when ("canister_init" `elem` exportedFunctions wasm_mod) $
      void $ withES esref es $
         invokeExport inst "canister_init" []
         -- TODO: Check no calls are made

    return (esref, cid, inst)
  case result of
    Left  err -> return $ Trap err
    Right raw_state -> return $ Return raw_state

rawQueryMethod :: RawState s -> MethodName -> EntityId -> Blob -> ST s (TrapOr Response)
rawQueryMethod (esref, cid, inst) method caller dat = do
  let es = (initalExecutionState cid inst)
            { params = Params
                { param_dat    = Just dat
                , param_caller = Just caller
                , reject_code  = 0
                , reject_message = ""
                }
            }
  result <- runExceptT $ withES esref es $
    invokeExport inst ("canister_query " ++ method) []
    -- TODO: Check no calls are made

  case result of
    Left err -> return $ Trap err
    Right (_, es')
      | Just r <- response es' -> return $ Return r
      | otherwise -> return $ Trap "No response"

rawUpdateMethod :: RawState s -> MethodName -> EntityId -> Blob -> ST s (TrapOr ([MethodCall], Maybe Response))
rawUpdateMethod (esref, cid, inst) method caller dat = do
  let es = (initalExecutionState cid inst)
            { params = Params
                { param_dat    = Just dat
                , param_caller = Just caller
                , reject_code  = 0
                , reject_message = ""
                }
            }

  result <- runExceptT $ withES esref es $
    invokeExport inst ("canister_update " ++ method) []
  case result of
    Left  err -> return $ Trap err
    Right (_, es') -> return $ Return (calls es', response es')

rawCallbackMethod :: RawState s -> Callback -> EntityId -> Response -> ST s (TrapOr ([MethodCall], Maybe Response))
rawCallbackMethod (esref, cid, inst) callback caller res = do
  let param_caller = Just caller
  let params = case res of
        Reply dat ->
          Params { param_dat = Just dat, param_caller, reject_code = 0, reject_message = "" }
        Reject (rc, reject_message) ->
          Params { param_dat = Nothing, param_caller, reject_code = rejectCode rc, reject_message }
  let es = (initalExecutionState cid inst) { params }

  let WasmClosure fun_idx env = case res of
        Reply {}  -> reply_callback callback
        Reject {} -> reject_callback callback

  result <- runExceptT $ withES esref es $
    invokeTable inst fun_idx [I32 env]
  case result of
    Left  err -> return $ Trap err
    Right (_, es') -> return $ Return (calls es', response es')

