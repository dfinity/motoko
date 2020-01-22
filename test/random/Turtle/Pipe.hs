module Turtle.Pipe (pipe, writeonlyblocking) where

import Turtle

-- for missing stuff
import qualified System.IO
import qualified GHC.IO.Handle.FD
import qualified Control.Exception
import qualified Data.Text.IO (hPutStrLn)
import qualified Control.Monad.Managed


-- Stuff that is missing from Turtle

-- | Stream lines of `Text` to a file, blocking

pipe :: MonadIO io => Turtle.FilePath -> Shell Line -> io ()
pipe file s = sh (do
    handle <- using (writeonlyblocking file)
    line   <- s
    liftIO (Data.Text.IO.hPutStrLn handle (lineToText line)) )


-- | Acquire a `Managed` blocking write-only `Handle` from a `FilePath`
writeonlyblocking :: Control.Monad.Managed.MonadManaged managed => Turtle.FilePath -> managed System.IO.Handle
writeonlyblocking file = using (managed (withBlockingTextFile file System.IO.WriteMode))


-- Stuff that is missing from Filesystem

-- | Open a file in text mode, and pass its 'Handle' to a provided
-- computation. The 'Handle' will be automatically closed when the
-- computation returns.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
withBlockingTextFile :: Turtle.FilePath -> System.IO.IOMode -> (System.IO.Handle -> IO a) -> IO a
withBlockingTextFile path mode = Control.Exception.bracket (openBlockingTextFile path mode) System.IO.hClose

-- | Open a file in text mode, and return an open 'Handle'. The 'Handle'
-- should be closed with 'IO.hClose' when it is no longer needed.
--
-- 'withBlockingTextFile' is easier to use, because it will handle the
-- 'Handle'&#x2019;s lifetime automatically.
--
-- This computation throws 'IOError' on failure. See &#8220;Classifying
-- I/O errors&#8221; in the "System.IO.Error" documentation for information on
-- why the failure occured.
openBlockingTextFile path = GHC.IO.Handle.FD.openFileBlocking (encodeString path)


-- See discussion https://mail.haskell.org/pipermail/haskell-cafe/2012-October/104030.html
