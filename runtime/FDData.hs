{-# LANGUAGE DeriveDataTypeable #-}

module FDData (ArithOp (..), FDConstraint (..), RelOp (..), LabelingStrategy (..), updateFDConstr) where

import Types
--import ExternalSolver
import Data.Typeable

-- ---------------------------------------------------------------------------
-- Finite Domain Constraint Representation
-- ---------------------------------------------------------------------------

data FDConstraint
  = FDRel RelOp (Term Int) (Term Int)
  | FDArith ArithOp (Term Int) (Term Int) (Term Int)
  | FDSum [Term Int] (Term Int)
  | FDAllDifferent [Term Int]
  | FDDomain [Term Int] (Term Int) (Term Int)
  | FDLabeling LabelingStrategy [Term Int] ID
 deriving (Eq,Show,Typeable)

data ArithOp 
  = Plus 
  | Minus 
  | Mult 
 deriving (Eq,Show)

data RelOp 
  = Equal
  | Diff
  | Less 
 deriving (Eq,Show)

-- Haskell-Type for LabelingStrategies

data LabelingStrategy = InOrder
                      | FirstFail
                      | MiddleOut
                      | EndsOut
 deriving (Eq,Ord,Show)

updateFDConstr :: Store m => (Term Int -> m (Term Int)) -> FDConstraint -> m FDConstraint
updateFDConstr update (FDRel relOp t1 t2) = do t1' <- update t1
                                               t2' <- update t2
                                               return $ FDRel relOp t1' t2'
updateFDConstr update (FDArith arithOp t1 t2 r) = do t1' <- update t1
                                                     t2' <- update t2
                                                     r'  <- update r
                                                     return $ FDArith arithOp t1' t2' r'
updateFDConstr update (FDSum vs r) = do vs' <- mapM update vs
                                        r'  <- update r
                                        return $ FDSum vs' r'
updateFDConstr update (FDAllDifferent vs) = do vs' <- mapM update vs
                                               return $ FDAllDifferent vs'
updateFDConstr update (FDDomain vs l u) = do vs' <- mapM update vs
                                             l'  <- update l
                                             u'  <- update u
                                             return $ FDDomain vs' l' u'
updateFDConstr update (FDLabeling s vs i) = do vs' <- mapM update vs
                                               return $ FDLabeling s vs' i


