{-# language OverloadedStrings, DuplicateRecordFields,
  ExplicitForAll, ScopedTypeVariables, BlockArguments,
  LambdaCase #-}

module Main where

import           Prelude hiding (log)

import qualified Control.Exception as Exception
import           Control.Lens ((^.))
import           Control.Monad (unless, guard)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bifunctor (first)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Language.LSP.Test hiding (message)
import           Language.LSP.Types (TextDocumentIdentifier(..), Position(..), HoverContents(..), MarkupContent(..), MarkupKind(..), TextEdit(..), Range(..), DidSaveTextDocumentParams(..), SMethod(..), Diagnostic(..), Location(..), Uri(..), filePathToUri, CompletionDoc(..))
import qualified Language.LSP.Types as LSP
import           Language.LSP.Types.Lens (contents, label, detail, documentation, message, additionalTextEdits, newText)
import           System.Directory (setCurrentDirectory, makeAbsolute, removeFile)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.IO (hPutStr, stderr)
import qualified Test.HUnit
import           Test.HUnit.Lang (HUnitFailure(..), formatFailureReason)
import           Test.Hspec (Expectation, shouldBe, shouldMatchList, shouldContain)

completionTestCase
  :: TextDocumentIdentifier
  -> Position
  -> ([(Text, Maybe Text, Maybe Text)] -> IO ())
  -> Session ()
completionTestCase doc pos pred = do
  actual <- getCompletions doc pos
  let unCompletionDoc (CompletionDocMarkup t) = unMarkup t
  liftIO (pred (map (\c -> (c^.label, c^.detail, fmap unCompletionDoc (c^.documentation))) actual))

hoverTestCase
  :: TextDocumentIdentifier
  -> Position
  -> Maybe Text
  -> Session ()
hoverTestCase doc pos expected = do
  actual <- getHover doc pos
  case fmap (^.contents) actual of
    Nothing
      | expected == Nothing ->
        pure ()
    Just (HoverContents (MarkupContent { _value = content}))
      | Just expected' <- expected ->
        if Text.isInfixOf expected' content
          then pure ()
          else do
            liftIO $ expectationFailure $
              "Hover content should've contained " <>
              show expected' <>
              " but was " <>
              show content
    hc ->
      liftIO $ expectationFailure $
        "Hover content should've been " <>
        show expected <>
        " but was: " <>
        show hc

definitionsTestCase
  :: FilePath
  -> TextDocumentIdentifier
  -> Position
  -> [(FilePath, Range)]
  -> Session ()
definitionsTestCase project doc pos expected = do
  LSP.InL response <- getDefinitions doc pos
  let expected' = map (first (filePathToUri . (project </>))) expected
  let actual = map (\(Location uri range) -> (uri, range)) response
  liftIO (shouldMatchList actual expected')

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

unMarkup :: MarkupContent -> Text
unMarkup (MarkupContent { _kind = MkMarkdown, _value = t}) = t

expectationFailure :: String -> Expectation
expectationFailure = Test.HUnit.assertFailure

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
  let serverCommand = mo_ide
        <> " --canister-main app.mo"
        <> " --debug"
        <> " --error-detail 0"
        <> " --package mydep " <> (project </> "mydependency")
        <> " --actor-idl " <> (project </> "idlpath")
        <> " --actor-alias counter aaaaa-aa"
  putStrLn "Starting server with: "
  putStrLn serverCommand
  handleHUnitFailure project do
    putStrLn "Starting the session"
    runSession
      serverCommand
      fullCaps
      "." $ do
        log "Initializing"
        initRes <- initializeResponse
        log "Hover tests"
        withDoc "ListClient.mo" \doc -> do
          hoverTestCase
            doc
            (Position 14 11)
            (Just "push : <T>(T, List<T>) -> List<T>")
          hoverTestCase
            doc
            (Position 17 11)
            (Just "pop : <T>List<T> -> (?T, List<T>)")
          hoverTestCase
            doc
            (Position 50 50)
            Nothing
        withDoc "app.mo" \doc -> do
          hoverTestCase
            doc
            (Position 8 39)
            (Just "natToNat8 : Nat -> Nat8")

        log "Definition tests"
        withDoc "definitions.mo" \doc -> do

          log "Definition for a function"
          definitionsTestCase
            project
            doc
            (Position 7 25)
            [("lib/list.mo", Range (Position 31 14) (Position 31 17))]

          log "Definition for a Class"
          definitionsTestCase
            project
            doc
            (Position 6 31)
            [("mydependency/lib.mo", Range (Position 5 17) (Position 5 24))]

          log "Definition for a function via an explicit field import"
          definitionsTestCase
            project
            doc
            (Position 8 15)
            [("lib/list.mo", Range (Position 56 14) (Position 56 18))]
          
          log "Definition for an imported module alias"
          definitionsTestCase
            project
            doc
            (Position 1 7)
            [("lib/list.mo", Range (Position 0 0) (Position 0 0))]
          
          log "Definition for an imported field"
          definitionsTestCase
            project
            doc
            (Position 2 9)
            [("lib/list.mo", Range (Position 56 14) (Position 56 18))]

        log "Completion tests"
        log "Completing top level definitions"
        withDoc "ListClient.mo" \doc -> do
          actual <- getCompletions doc (Position 7 0)
          liftIO
            (shouldBe
             (mapMaybe (\c -> guard (c^.label == "empty")
                              *> pure (c^.label, c^.detail)) actual)
             ([("empty", Just "() -> Stack")]))
          --     15 | List.push<Int>(x, s);
          -- ==> 15 | List.pus
          let edit = TextEdit (Range (Position 14 11) (Position 14 27)) "pus"
          _ <- applyEdit doc edit
          completionTestCase
            doc
          -- 15 | List.pus|
            (Position 14 14)
            (`shouldMatchList` [("push",Just "<T>(T, List<T>) -> List<T>", Just "Documentation for `push`")])

        log "Completing primitives"
        withDoc "ListClient.mo" \doc -> do
          let edit = TextEdit (Range (Position 15 0) (Position 15 0)) "Prim."
          _ <- applyEdit doc edit
          actual <- getCompletions doc (Position 15 6)
          liftIO
            (shouldBe
             (mapMaybe (\c -> guard (c^.label == "nat32ToNat")
                         *> pure (c^.label, c^.detail)) actual)
             ([("nat32ToNat", Just "Nat32 -> Nat")]))

        log "Completing not-yet-imported modules"
        withDoc "ListClient.mo" \doc -> do
          let edit = TextEdit (Range (Position 15 0) (Position 15 0)) "MyDep.print_"
          _ <- applyEdit doc edit
          [actual] <- getCompletions doc (Position 15 12)
          liftIO do
            shouldBe (actual^.label) "print_hello"
            shouldBe (actual^.detail) (Just "() -> Text (import from \"mo:mydep/lib\")")
            let Just (LSP.List [importEdit]) = actual^.additionalTextEdits
            shouldContain (Text.lines (importEdit^.newText)) ["import MyDep \"mo:mydep/lib\";"]

        log "Completing on not-yet-imported actors"
        withDoc "ListClient.mo" \doc -> do
          let edit = TextEdit (Range (Position 15 0) (Position 15 0)) "Counter.add_"
          _ <- applyEdit doc edit
          [actual] <- getCompletions doc (Position 15 12)
          liftIO do
            shouldBe (actual^.label) "add_counter"
            shouldBe (actual^.detail) (Just "shared Nat -> () (import from \"canister:counter\")")
            let Just (LSP.List [importEdit]) = actual^.additionalTextEdits
            shouldContain (Text.lines (importEdit^.newText)) ["import Counter \"canister:counter\";"]

        withDoc "ListClient.mo" \doc -> do
          --     1 | import List
          -- ==> 1 | ort List
          let edit = TextEdit (Range (Position 0 1) (Position 0 3)) ""
          _ <- applyEdit doc edit
          sendNotification STextDocumentDidSave (DidSaveTextDocumentParams doc Nothing)
          (diagnostic:_) <- waitForDiagnostics
          liftIO (diagnostic^.message `shouldBe` "unexpected token 'import'")

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
            sendNotification STextDocumentDidSave (DidSaveTextDocumentParams appDoc Nothing)
            diagnostic:_ <- waitForActualDiagnostics
            liftIO (diagnostic^.message `shouldBe` "unexpected token 'import'")

        log "Rebuilding with package paths"
        withDoc "app.mo" \doc -> do
          -- It knows how to handle package paths for rebuilding, and also
          -- for completions
          let edit = TextEdit (Range (Position 4 0) (Position 4 0)) "\nimport MyDep \"mo:mydep/broken\""
          _ <- applyEdit doc edit
          sendNotification STextDocumentDidSave (DidSaveTextDocumentParams doc Nothing)
          [diag] <- waitForActualDiagnostics
          liftIO (diag^.message `shouldBe` "operator is not defined for operand types\n  Text\nand\n  Nat")

        log "Completions from package paths"
        withDoc "app.mo" \doc -> do
          -- Imports the non-broken dependency module
          let edit = TextEdit (Range (Position 4 0) (Position 4 0)) "\nimport MyDep \"mo:mydep/lib\""
          _ <- applyEdit doc edit
          sendNotification STextDocumentDidSave (DidSaveTextDocumentParams doc Nothing)
          let edit2 = TextEdit (Range (Position 5 0) (Position 5 0)) "\nMyDep."
          _ <- applyEdit doc edit2
          completionTestCase
            doc
            -- MyDep.|
            (Position 6 6)
            (`shouldContain` [("print_hello", Just "() -> Text", Nothing)])

        log "Documentation in completions"
        withDoc "app.mo" \doc -> do
          let edit = TextEdit (Range (Position 4 0) (Position 4 0)) "\nimport Doc \"doc_comments\""
          _ <- applyEdit doc edit
          sendNotification STextDocumentDidSave (DidSaveTextDocumentParams doc Nothing)
          let edit2 = TextEdit (Range (Position 5 0) (Position 5 0)) "\nDoc."
          _ <- applyEdit doc edit2
          completionTestCase
            doc
            -- Doc.|
            (Position 6 4)
            (`shouldMatchList`
               [ ("e", Just "Nat", Just "I'm a doc comment")
               , ("e1", Just "Nat", Nothing)
               , ("e2", Just "Nat", Nothing)
               , ("e3", Just "Nat", Just "Edgecase 3\nEdgecase 3")
               , ("e4", Just "Nat", Just "Edgecase 4\nEdgecase 4")
               , ("e5", Just "Nat", Just "Edgecase 5\n\nEdgecase 5")
               , ("e6", Just "Nat", Just "")
               , ("e7", Just "Nat", Just "")
               ])
