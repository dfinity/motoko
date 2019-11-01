{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language ExplicitForAll #-}
{-# language ScopedTypeVariables #-}

module Main where

import qualified Control.Exception as Exception
import           Control.Lens ((^.))
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default
import           Data.Text (Text)
import           Language.Haskell.LSP.Test hiding (message)
import           Language.Haskell.LSP.Types (TextDocumentIdentifier(..), Position(..), HoverContents(..), MarkupContent(..), MarkupKind(..), TextEdit(..), Range(..), DidSaveTextDocumentParams(..), ClientMethod(..))
import           Language.Haskell.LSP.Types.Lens (contents, label, detail, message)
import           System.Directory (setCurrentDirectory)
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

plainMarkup :: Text -> Maybe HoverContents
plainMarkup t =
  Just
    (HoverContents MarkupContent
      { _kind = MkPlainText
      , _value = t
      })

handleHUnitFailure :: forall a. IO a -> IO a
handleHUnitFailure act = do
  result :: Either HUnitFailure a <- Exception.try act
  case result of
    Right res ->
      pure res
    Left (HUnitFailure _ reason) -> do
      hPutStr stderr (formatFailureReason reason)
      exitFailure

main :: IO ()
main = handleHUnitFailure $ do
  args <- getArgs
  unless (length args == 2)
    (putStrLn
      "This test expects two command line arguments,\
      \the path to the as-ide binary and the path to\
      \the test project it's supposed to run in")
  let [mo_ide, project] = args
  setCurrentDirectory project
  runSession (mo_ide <> " --argname app.mo") fullCaps "." $ do
    initRes <- initializeResponse
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
    --     1 | module {
    -- ==> 1 | ule {
    let edit = TextEdit (Range (Position 0 1) (Position 0 3)) ""
    _ <- applyEdit doc edit
    sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
    (diagnostic:_) <- waitForDiagnostics
    liftIO (diagnostic^.message `shouldBe` "unexpected token")
    closeDoc doc

    -- It finds errors in transitive modules that have been changed in
    -- the vfs but not yet stored to disc
    doc <- openDoc "ListClient.mo" "motoko"
    let edit = TextEdit (Range (Position 0 1) (Position 0 3)) ""
    _ <- applyEdit doc edit
    appDoc <- openDoc "app.mo" "motoko"
    sendNotification TextDocumentDidSave (DidSaveTextDocumentParams appDoc)
    (diagnostic:_) <- waitForDiagnostics
    liftIO (diagnostic^.message `shouldBe` "unexpected token")
