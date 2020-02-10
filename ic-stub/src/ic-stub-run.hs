{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Options.Applicative
import Control.Monad (join, forM_)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as B
import Control.Monad.Trans
import Control.Monad.Trans.State
import Text.Printf

import IC.Types
import IC.Stub
import IC.DRun.Parse (Ingress(..), parseFile)

type DRun = StateT IC IO

-- Pretty printing

dummyUserId :: CanisterId
dummyUserId = EntityId $ B.pack [0xCA, 0xFF, 0xEE]

printAsyncRequest :: AsyncRequest -> IO ()
printAsyncRequest CreateRequest{} =
    printf "→ create\n"
printAsyncRequest InstallRequest{} =
    printf "→ install\n"
printAsyncRequest UpgradeRequest{} =
    printf "→ upgrade\n"
printAsyncRequest (UpdateRequest _ _ method arg) =
    printf "→ update %s(%s)\n" method (prettyBlob arg)

printSyncRequest :: SyncRequest -> IO ()
printSyncRequest (StatusRequest rid) =
    printf "→ status? %s\n" (prettyBlob rid)
printSyncRequest (QueryRequest _ _ method arg) =
    printf "→ query %s(%s)\n" method (prettyBlob arg)

printReqStatus :: RequestStatus -> IO ()
printReqStatus Unknown =
    printf "← unknown\n"
printReqStatus Received =
    printf "← received\n"
printReqStatus Processing =
    printf "← processing\n"
printReqStatus (Rejected (c, s)) =
    printf "← rejected (%s): %s\n" (show c) s
printReqStatus (Completed CompleteUnit) =
    printf "← completed\n"
printReqStatus (Completed (CompleteCanisterId id)) =
    printf "← completed: canister-id = %s\n" (prettyID id)
printReqStatus (Completed (CompleteArg blob)) =
    printf "← completed: %s\n" (prettyBlob blob)

submitAndRun :: AsyncRequest -> DRun ReqResponse
submitAndRun r = do
    lift $ printAsyncRequest r
    submitRequest r
    runToCompletion
    r <- readRequest (StatusRequest (requestId r))
    lift $ printReqStatus r
    return r

submitRead :: SyncRequest -> DRun ReqResponse
submitRead r = do
    lift $ printSyncRequest r
    r <- readRequest r
    lift $ printReqStatus r
    return r

work :: FilePath -> IO ()
work msg_file = do
  msgs <- parseFile msg_file

  let user_id = dummyUserId

  flip evalStateT initialIC $
    forM_ msgs $ \case
      Install cid filename arg -> do
        wasm <- liftIO $ B.readFile filename
        _ <- submitAndRun (CreateRequest user_id (Just (EntityId cid)))
        submitAndRun (InstallRequest (EntityId cid) user_id wasm arg)
      Upgrade cid filename arg -> do
        wasm <- liftIO $ B.readFile filename
        submitAndRun (UpgradeRequest (EntityId cid) user_id wasm arg)
      Query  cid method arg -> submitRead  (QueryRequest (EntityId cid) user_id method arg)
      Update cid method arg -> submitAndRun (UpdateRequest (EntityId cid) user_id method arg)

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "Internet Computer Canister runner"
  <> progDesc "This runs an IC canister against a list of messages."
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$  strOption
            (  long "config"
            <> short 'c'
            <> metavar "CONFIG"
            <> value ""
            )
        <*> strArgument
            (  metavar "script"
            <> help "messages to execute"
            )
