{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Options.Applicative
import Control.Monad (join, foldM_, forM_)
import Data.Monoid ((<>))
import System.FilePath
import qualified Data.ByteString.Lazy as B
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Text.Hex as T
import Text.Printf

import IC.DRun.Parse (Type(..), parseFile)
import IC.Stub
  (Blob, EntityId,
   IC, AsyncRequest(..), SyncRequest(..), requestId, ReqResponse(..),
   RequestStatus(..), CompletionValue(..),
   initialIC, submitRequest, readRequest, runToCompletion)

type DRun = StateT IC IO

-- Pretty printing

prettyBlob :: Blob -> String
prettyBlob b = "0x" ++ T.unpack (T.encodeHex (B.toStrict b))

prettyID :: EntityId -> String
prettyID = prettyBlob -- implement the "ic:…" stuff

printAsyncRequest :: AsyncRequest -> IO ()
printAsyncRequest (InstallRequest _ _) =
    printf "→ install\n"
printAsyncRequest (UpdateRequest method arg) =
    printf "→ update %s(%s)\n" method (prettyBlob arg)

printSyncRequest :: SyncRequest -> IO ()
printSyncRequest (StatusRequest rid) =
    printf "→ status? %s\n" (prettyBlob rid)
printSyncRequest (QueryRequest method arg) =
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

work :: FilePath -> FilePath -> IO ()
work wasm_file msg_file = do
  wasm <- B.readFile wasm_file
  msgs <- parseFile msg_file

  flip evalStateT initialIC $ do
    req_res <- submitAndRun (InstallRequest wasm B.empty)
    forM_ msgs $ \case
       (Query,  method, arg) -> submitRead  (QueryRequest method arg)
       (Update, method, arg) -> submitAndRun (UpdateRequest method arg)

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
            (  metavar "*.wasm"
            <> help "Wasm module"
            )
        <*> strArgument
            (  metavar "script"
            <> help "messags to execute"
            )
