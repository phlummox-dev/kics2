module Solver.States (SolverStates (..), initStates, getSolverStates, setSolverStates
                     , EQStore, getEQStore, setEQStore, initEQState) where

import Solver.Overton.OvertonFD (FDState, initFDState)
import Solver.EQSolver.OvertonEQ (EQState, initEQState)

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

-- added for solving equational constraints using the overton solver

type EQStore = EQState

eqStore :: IORef EQStore
eqStore = unsafePerformIO $ newIORef initEQState

getEQStore :: IO EQStore
getEQStore = readIORef eqStore

setEQStore :: EQStore -> IO ()
setEQStore store' = modifyIORef eqStore (\store -> store')
