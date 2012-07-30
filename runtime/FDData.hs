{-# LANGUAGE DeriveDataTypeable #-}

module FDData (ArithOp (..), FDConstraint (..), RelOp (..), LabelingStrategy (..), updateFDConstr) where

import Types
import Data.Typeable

-- ---------------------------------------------------------------------------
-- Finite Domain Constraint Representation
-- ---------------------------------------------------------------------------

data FDConstraint
  = FDRel RelOp (FDTerm Int) (FDTerm Int)
  | FDArith ArithOp (FDTerm Int) (FDTerm Int) (FDTerm Int)
  | FDSum (FDList (FDTerm Int)) (FDTerm Int)
  | FDAllDifferent (FDList (FDTerm Int))
  | FDDomain (FDList (FDTerm Int)) (FDTerm Int) (FDTerm Int)
  | FDLabeling LabelingStrategy (FDList (FDTerm Int)) ID
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
  | LessEqual 
 deriving (Eq,Show)

-- Haskell-Type for LabelingStrategies

data LabelingStrategy = InOrder
                      | FirstFail
                      | MiddleOut
                      | EndsOut
 deriving (Eq,Ord,Show)

updateFDConstr :: Store m => (FDTerm Int -> m (FDTerm Int)) -> FDConstraint -> m FDConstraint
updateFDConstr update (FDRel relOp t1 t2) = do t1' <- update t1
                                               t2' <- update t2
                                               return $ FDRel relOp t1' t2'
updateFDConstr update (FDArith arithOp t1 t2 r) = do t1' <- update t1
                                                     t2' <- update t2
                                                     r'  <- update r
                                                     return $ FDArith arithOp t1' t2' r'
updateFDConstr update (FDSum (FDList i vs) r) = do vs' <- mapM update vs
                                                   r'  <- update r
                                                   return $ FDSum (FDList i vs') r'
updateFDConstr update (FDAllDifferent (FDList i vs)) = do vs' <- mapM update vs
                                                          return $ FDAllDifferent (FDList i vs')
updateFDConstr update (FDDomain (FDList i vs) l u) = do vs' <- mapM update vs
                                                        l'  <- update l
                                                        u'  <- update u
                                                        return $ FDDomain (FDList i vs') l' u'
updateFDConstr update (FDLabeling s (FDList i vs) j) = do vs' <- mapM update vs
                                                          return $ FDLabeling s (FDList i vs') j


