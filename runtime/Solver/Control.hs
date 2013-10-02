{-# LANGUAGE ExistentialQuantification #-}

module Solver.Control where

import Debug
import Solver.Constraints (FDConstraint)
import Solver.EquationSolver (Solution)
import Solver.Interface (processWith)
import Solver.OvertonUtils (overtonSolver)
import Types

data Solver m a = forall c . (WrappableConstraint c) 
  => Solver (Cover -> c -> a -> Solution m a)

-- list of supported solvers
solvers :: (Store m, NonDet a) => [Solver m a]
solvers = [Solver overton]

trySolver :: (Store m, NonDet a) => Cover -> [Solver m a] -> WrappedConstraint -> a -> Solution m a
trySolver _  []                         wc _   = internalError $ 
  "Constraint not solvable with supported solvers: " ++ (show wc)
trySolver cd ((Solver process):solvers) wc val = case unwrapCs wc of
  Nothing -> trySolver cd solvers wc val
  Just c  -> do c' <- updateVars cd c
                process cd c' val

solveAll :: (Store m, NonDet a) => Cover -> WrappedConstraint -> a -> Solution m a
solveAll cd = trySolver cd solvers

overton :: (Store m, NonDet a) => Cover -> FDConstraint -> a -> Solution m a
overton = processWith overtonSolver
