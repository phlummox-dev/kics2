{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DatatypeContexts #-}
{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}

module SolverControl where

import Types
import FDData
import MCPSolver

import ExternalSolver

import Debug.Trace as DT

data Solver = forall c . (WrappableConstraint c) => Solver ([c] -> Constraints)

-- filter heterogenous wrapped constraint list for constraints of a given type
filterCs :: WrappableConstraint c => [WrappedConstraint] -> ([c],[WrappedConstraint])
filterCs [] = ([],[])
filterCs (wc:wcs) = let (cs,wcs') = filterCs wcs
                    in case unwrapCs wc of Just c  -> (c:cs,wcs')
                                           Nothing -> (cs,wc:wcs')

-- list of supported constraint solvers
solvers :: [Solver]
solvers = [Solver runGecode, Solver runOverton]
--solvers = [Solver runOverton,Solver runGecode]

-- try to solve all constraints of the heterogenous list of wrappable constraints by filtering the list for constraints of the
-- supported solvers
-- if constraints of supported type are found, run the corresponding solver
-- otherwise try the next solver in the list
solveAll :: (Store m, NonDet a) => [WrappedConstraint] -> [Solver] -> a -> m a
solveAll wcs []                       _ = error $ "SolverControl.solveAll: Not solvable with supported solvers: " ++ show wcs
solveAll wcs ((Solver solve):solvers) e = case filterCs wcs of 
  ([],[])   -> return $ failCons 0 defFailInfo
  ([],wcs') -> solveAll wcs' solvers e
  (cs,[])   -> do updatedCs <- mapM updateVars cs
                  let bindings = solve updatedCs
                  return $ guardCons defCover bindings e
  (cs,wcs') -> do updatedCs <- mapM updateVars cs
                  let bindings = solve updatedCs
                  e' <- solveAll wcs' solvers e
                  return $ guardCons defCover bindings e'

-- Run the Gecode-Solver provided by the Monadic-Constraint-Programming-Framework
runGecode :: [FDConstraint] -> Constraints
runGecode = DT.trace "runGecode" $ \(cs :: [FDConstraint]) -> runSolver Gecode cs

-- Run the Overton-Solver provided by the Monadic-Constraint-Programming-Framework
runOverton :: [FDConstraint] -> Constraints
runOverton = DT.trace "runOverton" $ \(cs :: [FDConstraint]) -> runSolver Overton cs

-- Run the basic Overton-Solver
-- runBasicOverton :: [FDConstraint] -> Constraints
-- runBasicOverton = \(cs :: [FDConstraint]) -> runSolver OvertonSolver cs

