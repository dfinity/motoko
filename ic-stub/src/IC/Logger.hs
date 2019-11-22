module IC.Logger where

import Control.Monad.State

class Logger m where
    logTrap :: String -> m ()

instance (Monad m, Logger m) => Logger (StateT s m) where
    logTrap x = lift $ logTrap x

instance Logger IO where
    logTrap msg = putStrLn $ "Trap: " ++ msg
