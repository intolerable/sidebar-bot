module Control.Concurrent.VarThread where

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Monad.STM

-- add weak stuff
data VarThread a = VarThread (TVar (Maybe a)) (Async ())

newEmptyVarThread :: ((a -> STM ()) -> IO ()) -> IO (VarThread a)
newEmptyVarThread f = do
  t <- newTVarIO Nothing
  a <- async (f (writeTVar t . Just))
  return $ VarThread t a

readVarThread :: VarThread a -> STM a
readVarThread (VarThread t _) =
  readTVar t >>= \case
    Just x -> return x
    Nothing -> retry
