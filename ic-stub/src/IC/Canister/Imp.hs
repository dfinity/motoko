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
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import Data.STRef
import Data.Maybe

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

