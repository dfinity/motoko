{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Main where

import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types

import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.Text (Text)
import System.Directory (setCurrentDirectory)
import Test.Hspec (shouldBe)

hoverTestCase
  :: TextDocumentIdentifier
  -> Position
  -> Maybe HoverContents
  -> Session ()
hoverTestCase doc pos expected = do
  actual <- getHover doc pos
  liftIO (shouldBe (fmap _contents actual) expected)

plainMarkup :: Text -> Maybe HoverContents
plainMarkup t =
  Just
    (HoverContents MarkupContent
      { _kind = MkPlainText
      , _value = t
      })

main :: IO ()
main = do
  setCurrentDirectory "/home/creek/code/example-project"
  runSession "as-ide" fullCaps "." $ do
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
    pure ()

-- To make tests involving diagnostics work, we need the compiler
-- pipeline to read its files from the VFS

-- let edit = TextEdit (Range (Position 1 1) (Position 1 3)) ""
-- _ <- applyEdit doc edit
-- request_ TextDocumentDidSave (DidSaveTextDocumentParams doc)
-- diags <- waitForDiagnostics
