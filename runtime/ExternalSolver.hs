{-# LANGUAGE TypeFamilies #-}

module ExternalSolver where

import Types

-- ---------------------------------------------------------------------------
-- Integration of external fd constraint solvers
-- ---------------------------------------------------------------------------

-- By implementing this type class, one can integrate fd constraint solvers in KiCS2:
-- @type variable s - fd constraint solver
-- @type variable c - wrappable fd constraints, which can be solved by the given solver

class ExternalFDSolver solver where
  -- instance specific types helping with translating and solving the wrappable constraints:

  -- |Type of constraints, which may be solved by the solver
  type ForConstraint solver :: *

  -- |Type for representing the constraint modeling language of a specific solver
  -- may include additional information for solving constraints like labeling information
  type SolverModel solver :: *

  -- |Type for representing the solutions provided by a specific solver
  type Solutions solver :: *

  -- |Run a specific solver on a list of wrappable constraints
  -- The default implementation first updates the constraint variables
  -- regarding bindings introduced by (=:=).
  -- Then the constraints are translated into a solver specific modeling language
  -- which is solved afterwards.
  -- Finally the solutions provided by the specific solver are
  -- transformed into (constraint) variable bindings
  -- (i.e. by constructing guard expressions with binding constraints
  -- calling bindSolution)
  runSolver :: solver -> [ForConstraint solver] -> Constraints
  runSolver solver wcs = let model     = translate solver wcs
                             solutions = solveWith solver model
                         in makeBindings solver solutions

  -- |Translate given list of wrappable constraints into the modeling language of
  -- a specific solver and collect labeling information
  translate :: solver -> [ForConstraint solver] -> SolverModel solver

  -- |Solve the given solver model using the collected labeling information
  solveWith :: solver -> SolverModel solver -> Solutions solver

  -- |Transform solutions provided by a specific solver into bindings for
  -- the occurring constraint variables
  makeBindings :: solver -> Solutions solver -> Constraints
