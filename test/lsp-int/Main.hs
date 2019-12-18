{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language ExplicitForAll #-}
{-# language ScopedTypeVariables #-}

module Main where

import           Prelude hiding (log)

import qualified Control.Exception as Exception
import           Control.Lens ((^.))
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default
import           Data.Text (Text)
import           Language.Haskell.LSP.Test hiding (message)
import           Language.Haskell.LSP.Types (TextDocumentIdentifier(..), Position(..), HoverContents(..), MarkupContent(..), MarkupKind(..), TextEdit(..), Range(..), DidSaveTextDocumentParams(..), ClientMethod(..), Diagnostic(..))
import           Language.Haskell.LSP.Types.Lens (contents, label, detail, message)
import           System.Directory (setCurrentDirectory, makeAbsolute)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStr, stderr)
import           Test.HUnit.Lang (HUnitFailure(..), formatFailureReason)
import           Test.Hspec (shouldBe, shouldMatchList)

completionTestCase
  :: TextDocumentIdentifier
  -> Position
  -> [(Text, Maybe Text)]
  -> Session ()
completionTestCase doc pos expected = do
  actual <- getCompletions doc pos
  liftIO
    (shouldMatchList
      (map (\c -> (c^.label, c^.detail)) actual)
      expected)

hoverTestCase
  :: TextDocumentIdentifier
  -> Position
  -> Maybe HoverContents
  -> Session ()
hoverTestCase doc pos expected = do
  actual <- getHover doc pos
  liftIO (shouldBe (fmap (^.contents) actual) expected)

-- | Discards all empty diagnostic reports (as those are merely used
-- to clear out old reports)
waitForActualDiagnostics :: Session [Diagnostic]
waitForActualDiagnostics = do
  diags <- waitForDiagnostics
  if null diags then waitForActualDiagnostics else pure diags

plainMarkup :: Text -> Maybe HoverContents
plainMarkup t =
  Just
    (HoverContents MarkupContent
      { _kind = MkPlainText
      , _value = t
      })

handleHUnitFailure :: forall a. String -> IO a -> IO a
handleHUnitFailure project act = do
  result :: Either HUnitFailure a <- Exception.try act
  case result of
    Right res ->
      pure res
    Left (HUnitFailure _ reason) -> do
      hPutStr stderr =<< readFile (project <> "/ls.log")
      hPutStr stderr (formatFailureReason reason)
      exitFailure

log :: String -> Session ()
log = liftIO . putStrLn

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 2)
    (putStrLn
      "This test expects two command line arguments,\
      \the path to the mo-ide binary and the path to\
      \the test project it's supposed to run in")
  let [mo_ide, project] = args
  project <- makeAbsolute project
  setCurrentDirectory project
  handleHUnitFailure project $ do
    putStrLn "Starting the session"
    runSession
      (mo_ide
       <> " --canister-main app.mo --debug"
       <> " --package mydep " <> project <> "/mydependency/")
      fullCaps
      "." $ do
        log "Initializing"
        initRes <- initializeResponse
        log "Hover tests"
        doc <- openDoc "ListClient.mo" "motoko"
        hoverTestCase
          doc
          (Position 13 11)
          (plainMarkup "push : <T>(T, List<T>) -> List<T>")
        hoverTestCase
          doc
          (Position 16 11)
          (plainMarkup "pop : <T>List<T> -> (?T, List<T>)")
        hoverTestCase
          doc
          (Position 50 50)
          Nothing
        log "Completion tests"
        --     14 | List.push<Int>(x, s);
        -- ==> 14 | List.pus
        let edit = TextEdit (Range (Position 13 11) (Position 13 27)) "pus"
        _ <- applyEdit doc edit
        completionTestCase
          doc
        -- 14 | List.pus|
          (Position 13 14)
          [("push",Just "<T>(T, List<T>) -> List<T>")]
        closeDoc doc
        doc <- openDoc "ListClient.mo" "motoko"
        --     1 | import List
        -- ==> 1 | ort List
        let edit = TextEdit (Range (Position 0 1) (Position 0 3)) ""
        _ <- applyEdit doc edit
        sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
        (diagnostic:_) <- waitForDiagnostics
        liftIO (diagnostic^.message `shouldBe` "unexpected token")
        closeDoc doc

        log "Lexer failures don't crash the server"
        doc <- openDoc "ListClient.mo" "motoko"
        -- Creates an unclosed text literal, which triggers a lexer error
        let edit = TextEdit (Range (Position 0 1) (Position 0 3)) "\"hello"
        _ <- applyEdit doc edit
        -- We're just testing that the server doesn't crash here
        getCompletions doc (Position 0 0)
        getHover doc (Position 0 0)
        closeDoc doc

        log "Finds errors in non-saved files"
        -- It finds errors in transitive modules that have been changed in
        -- the vfs but not yet stored to disc
        doc <- openDoc "ListClient.mo" "motoko"
        let edit = TextEdit (Range (Position 0 1) (Position 0 3)) ""
        _ <- applyEdit doc edit
        appDoc <- openDoc "app.mo" "motoko"
        sendNotification TextDocumentDidSave (DidSaveTextDocumentParams appDoc)
        diagnostic:_ <- waitForActualDiagnostics
        liftIO (diagnostic^.message `shouldBe` "unexpected token")
        closeDoc doc
        closeDoc appDoc

        log "Rebuilding with package paths"
        -- It knows how to handle package paths for rebuilding, and also
        -- for completions
        doc <- openDoc "app.mo" "motoko"
        let edit = TextEdit (Range (Position 1 0) (Position 1 0)) "\nimport MyDep \"mo:mydep/broken.mo\""
        _ <- applyEdit doc edit
        sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
        [diag] <- waitForActualDiagnostics
        liftIO (diag^.message `shouldBe` "operator not defined for operand types\n  Text\nand\n  Nat")

        log "Completions from package paths"
        -- Imports the non-broken dependency module
        let edit2 = TextEdit (Range (Position 2 0) (Position 3 0)) "\nimport MyDep \"mo:mydep/lib.mo\""
        _ <- applyEdit doc edit2
        sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
        let edit3 = TextEdit (Range (Position 3 0) (Position 3 0)) "\nMyDep."
        _ <- applyEdit doc edit3
        completionTestCase
          doc
          -- MyDep.|
          (Position 4 6)
          [("print_hello", Just "() -> Text")]
