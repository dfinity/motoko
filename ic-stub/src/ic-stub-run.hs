{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Options.Applicative
import Control.Monad (join, foldM_, forM_)
import Data.Monoid ((<>))
import System.FilePath
import qualified Data.ByteString.Lazy as B
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import IC.DRun.Parse (Type(..), parseFile)
import IC.Stub
  (ICT, IC, AsyncRequest(..), SyncRequest(..), requestId, ReqResponse(..),
   initialIC, submitRequest, readRequest, runToCompletion)

type DRun = StateT IC IO

submitAndRun :: AsyncRequest -> DRun ReqResponse
submitAndRun r = do
    submitRequest r
    runToCompletion
    readRequest (StatusRequest (requestId r))

work :: FilePath -> FilePath -> IO ()
work wasm_file msg_file = do
  wasm <- B.readFile wasm_file
  msgs <- parseFile msg_file

  flip evalStateT initialIC $ do
    req_res <- submitAndRun (InstallRequest wasm B.empty)
    liftIO $ print req_res -- assert completed
    forM_ msgs $ \case
       (Query, method, arg) ->
           readRequest (QueryRequest method arg) >>= liftIO . print
       (Update, method, arg) ->
           submitAndRun (UpdateRequest method arg) >>= liftIO . print

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
