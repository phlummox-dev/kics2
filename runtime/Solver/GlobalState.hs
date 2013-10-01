module Solver.GlobalState (getFDState, setFDState) where

import Solver.OvertonFD (FDState, initState)

import Control.Monad (liftM)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import System.IO.Unsafe (unsafePerformIO)

data GlobalState = GS {
  fdState :: FDState
}

gs :: IORef GlobalState
gs = unsafePerformIO $ newIORef (GS initState)

getFDState :: IO FDState
getFDState = liftM fdState (readIORef gs)

setFDState :: FDState -> IO ()
setFDState state = modifyIORef gs (\s -> s { fdState = state })
