{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DatatypeContexts #-}
{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}

module SolverControl where

import Types
import FDData
import {-# SOURCE #-} MCPSolver -- necessary to prevent import cycle

import ExternalSolver

data (Store m, NonDet a) => Solver m a = forall c . (WrappableConstraint c) => Solver ([c] -> a -> m a)

-- filter heterogenous wrapped constraint list for constraints of a given type
filterCs :: WrappableConstraint c => [WrappedConstraint] -> ([c],[WrappedConstraint])
filterCs [] = ([],[])
filterCs (wc:wcs) = let (cs,wcs') = filterCs wcs
                    in case unwrapCs wc of Just c  -> (c:cs,wcs')
                                           Nothing -> (cs,wc:wcs')

-- list of supported constraint solvers
solvers :: (Store m, NonDet a) => [Solver m a]
solvers = [Solver runGecode, Solver runOverton]
--solvers = [Solver runOverton,Solver runGecode]

-- try to solve all constraints of the heterogenous list of wrappable constraints by running the supported solvers
-- one after another
solveAll :: (Store m, NonDet a) => [WrappedConstraint] -> [Solver m a] -> a -> m a
solveAll wcs []                       _ = error $ "SolverControl.solveAll: Not solvable with supported solvers: " ++ show wcs
solveAll wcs ((Solver solve):solvers) e = case filterCs wcs of ([],[])   -> return $ failCons 0 defFailInfo
                                                               ([],wcs') -> solveAll wcs' solvers e
                                                               (cs,[])   -> solve cs e
                                                               (cs,wcs') -> do e' <- solve cs e
                                                                               solveAll wcs' solvers e'

-- Run the Gecode-Solver provided by the Monadic-Constraint-Programming-Framework
runGecode :: (Store m, NonDet a) => [FDConstraint] -> a -> m a
runGecode = \(cs :: [FDConstraint]) e -> runSolver Gecode cs e

-- Run the Overton-Solver provided by the Monadic-Constraint-Programming-Framework
runOverton :: (Store m, NonDet a) => [FDConstraint] -> a -> m a
runOverton = \(cs :: [FDConstraint]) e -> runSolver Overton cs e
