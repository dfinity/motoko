{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Main where

import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.Text (Text)
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types (TextDocumentIdentifier(..), Position(..), HoverContents(..), MarkupContent(..), MarkupKind(..), TextEdit(..), Range(..))
import Language.Haskell.LSP.Types.Lens (contents, label, detail)
import System.Directory (setCurrentDirectory)
import System.Environment (getArgs)
import Test.Hspec (shouldBe, shouldMatchList)

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

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 2)
    (putStrLn
      "This test expects two command line arguments,\
      \the path to the as-ide binary and the path to\
      \the test project it's supposed to run in")
  let [as_ide, project] = args
  setCurrentDirectory project
  runSession as_ide fullCaps "." $ do
    initRes <- initializeResponse
    doc <- openDoc "ListClient.as" "actorscript"
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
    pure ()



-- To make tests involving diagnostics work, we need the compiler
-- pipeline to read its files from the VFS

-- let edit = TextEdit (Range (Position 1 1) (Position 1 3)) ""
-- _ <- applyEdit doc edit
-- request_ TextDocumentDidSave (DidSaveTextDocumentParams doc)
-- diags <- waitForDiagnostics
