{-# language OverloadedStrings, {-PartialTypeSignatures,-} ScopedTypeVariables, TupleSections #-}

module Embedder (Embedder(..), WasmAPI(..), embedder, embedderCommand, addCompilerArgs, addEmbedderArgs, invokeEmbedder) where

import Test.QuickCheck

import Turtle
import Turtle.Pipe

import Control.Monad.Catch
import GHC.IO.Exception (IOException)
import Data.IORef

data WasmAPI = DontPrint | WASI

data Embedder = Reference | WasmTime WasmAPI | Drun

instance Arbitrary Embedder where arbitrary = elements [Reference, WasmTime DontPrint, WasmTime WASI, Drun]
instance Arbitrary WasmAPI where arbitrary = elements [DontPrint, WASI]

embedderCommand Reference = "wasm"
embedderCommand (WasmTime _) = "wasmtime"
embedderCommand Drun = "drun"

addCompilerArgs Reference = ("-no-system-api" :)
addCompilerArgs (WasmTime DontPrint) = ("-no-system-api" :)
addCompilerArgs (WasmTime WASI) = ("-wasi-system-api" :)
addCompilerArgs Drun = id

addEmbedderArgs Reference = id
addEmbedderArgs (WasmTime _) = ("--disable-cache" :) . ("--cranelift" :)
addEmbedderArgs Drun = ("--extra-batches" :) . ("10" :)

invokeEmbedder :: Embedder -> Turtle.FilePath -> IO (ExitCode, Text, Text)
invokeEmbedder embedder wasm = go embedder
  where fileArg = fromString . encodeString
        Right wasmFile = toText wasm
        revconcating = Fold (flip (:)) [] id
        go :: Embedder -> IO (ExitCode, Text, Text)
        go Drun = do
          fuzz <- newIORef ""

          sh $ do
            let Right w = toText wasm
                control = wasm <.> "fifo"
            rm (fileArg control) `catch` \(_ :: GHC.IO.Exception.IOException) -> pure () -- rm -f
            let Right c = toText control
            procs "mkfifo" [c] empty
            consumer <- forkShell $ inshell ("drun --extra-batches 100 " <> c) empty
            let install = unsafeTextToLine $ format ("install ic:2A012B "%s%" 0x") w

            pipe (fileArg control) (pure install
                                   <|> "ingress ic:2A012B do 0x4449444c0000")
            lns <- wait consumer
            view lns
            let errors = grep (has "Err: " <|> has "Reject: ") lns
            linesToText . reverse <$> fold errors revconcating >>= liftIO <$> writeIORef fuzz

          (ExitSuccess, "",) <$> readIORef fuzz

        go _ = procStrictWithErr (embedderCommand embedder) (addEmbedderArgs embedder [wasmFile]) empty
        -- forkShell :: Show a => Shell a -> Shell (Async (Shell a))
        forkShell shell = do a <- fork (fold shell revconcating)
                             pure (select . reverse <$> a)


embedder :: Embedder
embedder = WasmTime DontPrint
