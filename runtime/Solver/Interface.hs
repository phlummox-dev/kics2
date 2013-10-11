{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Solver.Interface where

import Types

type Solution m a = m (Maybe (m (), a))

mkSolution :: Monad m => a -> Solution m a
mkSolution a = return $ Just (return (), a)

noSolution :: Monad m => Solution m a
noSolution = return Nothing

data ConstraintSolver state = Solver { 
  getState      :: Store m => m state,
  setState      :: Store m => state -> m ()
}

class ExternalSolver solver where

  type ForConstraint solver :: *

  processWith :: (Store m, NonDet a) => solver -> Cover -> ForConstraint solver -> a -> Solution m a

