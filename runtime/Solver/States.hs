module Solver.States (SolverStates (..), initStates, getSolverStates, setSolverStates) where

import Solver.Overton.OvertonFD (FDState, initFDState)

import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import System.IO.Unsafe (unsafePerformIO)

data SolverStates = States {
  fdState :: FDState
}

initStates :: SolverStates
initStates = States {
  fdState = initFDState
}

solverStates :: IORef SolverStates
solverStates = unsafePerformIO $ newIORef initStates

getSolverStates :: IO SolverStates
getSolverStates = readIORef solverStates

setSolverStates :: SolverStates -> IO ()
setSolverStates newSS = modifyIORef solverStates (\ss -> newSS)
