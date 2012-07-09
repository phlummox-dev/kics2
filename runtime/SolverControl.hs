{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DatatypeContexts #-}
{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}

module SolverControl where

import Types
import FDData
import {-# SOURCE #-} MCPSolver
import Debug.Trace

import ExternalSolver

data (Store m, NonDet a) => Solver m a = forall c . (ExternalConstraint c) => Solver ([c] -> a -> m a)

-- filter heterogenous external constraint list for constraints of a given type
filterCs :: ExternalConstraint c => [ExtConstraint] -> ([c],[ExtConstraint])
filterCs [] = ([],[])
filterCs (ec:ecs) = let (cs,ecs') = filterCs ecs
                    in case unwrapCs ec of Just c  -> (c:cs,ecs')
                                           Nothing -> (cs,ec:ecs')

-- list of supported constraint solvers
solvers :: (Store m, NonDet a) => [Solver m a]
solvers = [Solver runGecode, Solver runOverton]
--solvers = [Solver runOverton,Solver runGecode]

-- try solving all constraints of the heterogenous list of external constraints by running the supported solvers
-- one after another
solveAll :: (Store m, NonDet a) => [ExtConstraint] -> [Solver m a] -> a -> m a
solveAll extCs []                       _ = error $ "SolverControl.solveAll: Not solvable with supported solvers: " ++ show extCs
solveAll extCs ((Solver solve):solvers) e = case filterCs extCs of ([],[])  -> return $ failCons 0 defFailInfo
                                                                   ([],ecs) -> solveAll ecs solvers e
                                                                   (cs,[])  -> solve cs e
                                                                   (cs,ecs) -> do e' <- solveAll ecs solvers e
                                                                                  solve cs e'

-- Run the Gecode Solver provided by the Monadic Constraint Programming Framework
runGecode :: (Store m, NonDet a) => [FDConstraint] -> a -> m a
runGecode = trace "Running Gecode Solver" $ \(cs :: [FDConstraint]) e -> runSolver Gecode cs e

-- Run the Overton Solver provided by the Monadic Constraint Programming Framework
runOverton :: (Store m, NonDet a) => [FDConstraint] -> a -> m a
runOverton = trace "Running Overton Solver" $ \(cs :: [FDConstraint]) e -> runSolver Overton cs e
