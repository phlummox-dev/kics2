{-# LANGUAGE TypeFamilies #-} -- added for typeclass Constrainable and ExternalSolver

module ExternalSolver where

import Types

-- ---------------------------------------------------------------------------
-- Integration of external constraint solvers
-- ---------------------------------------------------------------------------

-- By implementing this type class, one can integrate constraint solvers in KiCS2:
-- @type variable s - constraint solver
-- @type variable c - wrappable constraints, which can be solved by the given solver

class WrappableConstraint c => ExternalFDSolver s c where	-- needs MultiParamTypeClasses and TypeFamilies

  -- instance specific types helping with translating and solving the external constraints:
  -- |Type for representing the given constraints as a model for a specific solver
  data SolverModel s c :: *
  -- |Type for representing labeling information collected in the translation progress
  -- like the chosen labeling strategy or the labeling variables
  data LabelInfo s c :: *
  -- |Type for representing the solutions provided by a specific solver
  data Solutions s c :: *

  -- |Run a specific solver on a list of external constraints
  -- The default implementation first updates the constraint variables
  -- regarding bindings introduced by (=:=).
  -- Then the constraints are translated into a solver specific model
  -- which is solved afterwards.
  -- Finally the solutions provided by the specific solver are
  -- transformed into (constraint) variable bindings
  -- (i.e. by constructing guard expressions with binding constraints
  -- calling bindSolution)
  runSolver :: (Store m, NonDet a) => s -> [c] -> a -> m a
  runSolver solver extCs e = do updatedCs <- mapM updateVars extCs
                                let (solverCs,info) = translate solver updatedCs
                                    results = solveWith solver solverCs info
                                return $ makeConstrSolutions solver results e

  -- |Translate given list of external constraints into a specific solver model
  -- and collect labeling information
  translate :: s -> [c] -> (SolverModel s c, LabelInfo s c)

  -- |Solve the given solver model using the collected labeling information
  solveWith :: s -> SolverModel s c -> LabelInfo s c -> Solutions s c

  -- |Transform solutions provided by a specific solver into bindings for
  -- the occurring constraint variables
  makeConstrSolutions :: NonDet a => s -> Solutions s c -> a  -> a
