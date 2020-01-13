{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language ExplicitForAll #-}
{-# language ScopedTypeVariables #-}
{-# language BlockArguments #-}

module Main where

import           Prelude hiding (log)

import qualified Control.Exception as Exception
import           Control.Lens ((^.))
import           Control.Monad (unless, guard)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Language.Haskell.LSP.Test hiding (message)
import           Language.Haskell.LSP.Types (TextDocumentIdentifier(..), Position(..), HoverContents(..), MarkupContent(..), MarkupKind(..), TextEdit(..), Range(..), DidSaveTextDocumentParams(..), ClientMethod(..), Diagnostic(..))
import           Language.Haskell.LSP.Types.Lens (contents, label, detail, message)
import           System.Directory (setCurrentDirectory, makeAbsolute, removeFile)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
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

-- | Brackets an action with the closing and opening of a TextDocument
withDoc :: String -> (TextDocumentIdentifier -> Session a) -> Session a
withDoc path action = do
  doc <- openDoc path "motoko"
  res <- action doc
  closeDoc doc
  pure res

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
  removeFile (project </> "ls.log") `Exception.catch` \(_ :: Exception.SomeException) -> pure ()
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
        withDoc "ListClient.mo" \doc -> do
          hoverTestCase
            doc
            (Position 14 11)
            (plainMarkup "push : <T>(T, List<T>) -> List<T>")
          hoverTestCase
            doc
            (Position 17 11)
            (plainMarkup "pop : <T>List<T> -> (?T, List<T>)")
          hoverTestCase
            doc
            (Position 50 50)
            Nothing
        withDoc "app.mo" \doc -> do
          hoverTestCase
            doc
            (Position 7 39)
            (plainMarkup "natToWord8 : Nat -> Word8")
        log "Completion tests"
        -- Completing top level definitions:
        withDoc "ListClient.mo" \doc -> do
          actual <- getCompletions doc (Position 7 0)
          liftIO
            (shouldBe
             ([("empty", Just "() -> Stack")])
             (mapMaybe (\c -> guard (c^.label == "empty")
                         *> pure (c^.label, c^.detail)) actual))
          --     15 | List.push<Int>(x, s);
          -- ==> 15 | List.pus
          let edit = TextEdit (Range (Position 14 11) (Position 14 27)) "pus"
          _ <- applyEdit doc edit
          completionTestCase
            doc
          -- 15 | List.pus|
            (Position 14 14)
            [("push",Just "<T>(T, List<T>) -> List<T>")]

        -- Completing primitives:
        withDoc "ListClient.mo" \doc -> do
          let edit = TextEdit (Range (Position 15 0) (Position 15 0)) "Prim."
          _ <- applyEdit doc edit
          actual <- getCompletions doc (Position 15 6)
          liftIO
            (shouldBe
             (mapMaybe (\c -> guard (c^.label == "word32ToNat")
                         *> pure (c^.label, c^.detail)) actual)
             ([("word32ToNat", Just "Word32 -> Nat")]))

        withDoc "ListClient.mo" \doc -> do
          --     1 | import List
          -- ==> 1 | ort List
          let edit = TextEdit (Range (Position 0 1) (Position 0 3)) ""
          _ <- applyEdit doc edit
          sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
          (diagnostic:_) <- waitForDiagnostics
          liftIO (diagnostic^.message `shouldBe` "unexpected token")

        log "Lexer failures don't crash the server"
        withDoc "ListClient.mo" \doc -> do
          -- Creates an unclosed text literal, which triggers a lexer error
          let edit = TextEdit (Range (Position 0 1) (Position 0 3)) "\"hello"
          _ <- applyEdit doc edit
          -- We're just testing that the server doesn't crash here
          getCompletions doc (Position 0 0)
          getHover doc (Position 0 0)

        log "Finds errors in non-saved files"
        withDoc "ListClient.mo" \doc -> do
          -- It finds errors in transitive modules that have been changed in
          -- the vfs but not yet stored to disc

          let edit = TextEdit (Range (Position 0 1) (Position 0 3)) ""
          _ <- applyEdit doc edit
          withDoc "app.mo" \appDoc -> do
            sendNotification TextDocumentDidSave (DidSaveTextDocumentParams appDoc)
            diagnostic:_ <- waitForActualDiagnostics
            liftIO (diagnostic^.message `shouldBe` "unexpected token")

        log "Rebuilding with package paths"
        withDoc "app.mo" \doc -> do
          -- It knows how to handle package paths for rebuilding, and also
          -- for completions
          let edit = TextEdit (Range (Position 3 0) (Position 3 0)) "\nimport MyDep \"mo:mydep/broken.mo\""
          _ <- applyEdit doc edit
          sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
          [diag] <- waitForActualDiagnostics
          liftIO (diag^.message `shouldBe` "operator not defined for operand types\n  Text\nand\n  Nat")

        log "Completions from package paths"
        withDoc "app.mo" \doc -> do
          -- Imports the non-broken dependency module
          let edit2 = TextEdit (Range (Position 3 0) (Position 3 0)) "\nimport MyDep \"mo:mydep/lib.mo\""
          _ <- applyEdit doc edit2
          sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
          let edit3 = TextEdit (Range (Position 4 0) (Position 4 0)) "\nMyDep."
          _ <- applyEdit doc edit3
          completionTestCase
            doc
            -- MyDep.|
            (Position 5 6)
            [("print_hello", Just "() -> Text")]
