{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Solver.Interface where

import Solver.EquationSolver (Solution)
import Types

data ConstraintSolver state = Solver { 
  getState      :: Store m => m state,
  setState      :: Store m => state -> m ()
}

class ExternalSolver solver where

  type ForConstraint solver :: *

  processWith :: (Store m, NonDet a) => solver -> Cover -> ForConstraint solver -> a -> Solution m a

