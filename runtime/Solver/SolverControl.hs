{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}

module Solver.SolverControl where

import FailInfo (defFailInfo)
import Solver.Constraints (FDConstraint, BConstraint)
import Solver.ExternalSolver
import Solver.OvertonSolver (OvertonSolver)
import Solver.SatSolver (SatSolver)
import Types

#ifdef GECODE
import Solver.GecodeSolver (GecodeSearchSolver, GecodeRuntimeSolver)
#endif

data Solver = forall c s. (WrappableConstraint c, ExternalSolver s) 
  => Solver (Cover -> [c] -> s Constraints)

-- filter heterogenous wrapped constraint list for constraints of a given type
filterCs :: WrappableConstraint c => [WrappedConstraint] 
         -> ([c],[WrappedConstraint])
filterCs [] = ([],[])
filterCs (wc:wcs) = let (cs,wcs') = filterCs wcs
                    in case unwrapCs wc of Just c  -> (c:cs,wcs')
                                           Nothing -> (cs,wc:wcs')

-- list of supported constraint solvers
solvers :: [Solver]
#ifdef GECODE
solvers = [Solver gecodeSearch, Solver overton, Solver sat]
#else
solvers = [Solver overton, Solver sat]
#endif

-- try to solve all constraints of the heterogenous list of wrappable
-- constraints by filtering the list for constraints of the supported solvers
-- if constraints of supported type are found, run the corresponding solver
-- otherwise try the next solver in the list
solveAll :: (Store m, NonDet a) => Cover -> [WrappedConstraint] -> [Solver] -> a -> m a
solveAll _  wcs []                        _ = error $ 
  "SolverControl.solveAll: Not solvable with supported solvers: " ++ show wcs
solveAll cd wcs ((Solver solver):solvers) e = case filterCs wcs of 
  ([],[])   -> return $ failCons cd defFailInfo
  ([],wcs') -> solveAll cd wcs' solvers e
  (cs,[])   -> do updatedCs <- mapM (updateVars cd) cs
                  let bindings = run $ solver cd updatedCs
                  return $ guardCons cd bindings e
  (cs,wcs') -> do updatedCs <- mapM (updateVars cd) cs
                  let bindings = run $ solver cd updatedCs
                  e' <- solveAll cd wcs' solvers e
                  return $ guardCons cd bindings e'

#ifdef GECODE
-- Run the Gecode-Runtime-Solver provided by the MCP framework
gecodeRuntime :: Cover -> [FDConstraint] -> GecodeRuntimeSolver Constraints
gecodeRuntime = eval

-- Run the Gecode-Search-Solver provided by the MCP framework
gecodeSearch :: Cover -> [FDConstraint] -> GecodeSearchSolver Constraints
gecodeSearch = eval
#endif

-- Run the Overton-Solver provided by the MCP framework
overton :: Cover -> [FDConstraint] -> OvertonSolver Constraints
overton = eval

-- Run the SAT-Solver by Sebastian Fischer
sat :: Cover -> [BConstraint] -> SatSolver Constraints
sat = eval
