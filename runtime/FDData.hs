{-# LANGUAGE DeriveDataTypeable #-}

--module FDData (ArithOp (..), FDConstraint (..), RelOp (..), LabelingStrategy (..), updateFDConstr) where
module FDData ( ArithOp (..)
              , FDConstraint (..)
              , RelOp (..)
              , LabelingStrategy (..)
              , BConstraint (..)
              , Junctor (..)
              , updateFDConstr, updateBConstr
              ) where

import Types
import PrimTypes
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
  | LessEqual 
 deriving (Eq,Show)

-- Haskell-Type for LabelingStrategies
data LabelingStrategy
  = InOrder
  | FirstFail
  | MiddleOut
  | EndsOut
 deriving (Eq,Ord,Show)

-- update all fd terms of a fd constraint with given update function
updateFDConstr :: Store m => (Term Int -> m (Term Int)) -> FDConstraint -> m FDConstraint
updateFDConstr update (FDRel relOp t1 t2) = do
  t1' <- update t1
  t2' <- update t2
  return $ FDRel relOp t1' t2'
updateFDConstr update (FDArith arithOp t1 t2 r) = do
  t1' <- update t1
  t2' <- update t2
  r'  <- update r
  return $ FDArith arithOp t1' t2' r'
updateFDConstr update (FDSum vs r) = do
  vs' <- mapM update vs
  r'  <- update r
  return $ FDSum vs' r'
updateFDConstr update (FDAllDifferent vs) = do
  vs' <- mapM update vs
  return $ FDAllDifferent vs'
updateFDConstr update (FDDomain vs l u) = do
  vs' <- mapM update vs
  l'  <- update l
  u'  <- update u
  return $ FDDomain vs' l' u'
updateFDConstr update (FDLabeling s vs j) = do
  vs' <- mapM update vs
  return $ FDLabeling s vs' j

-- ---------------------------------------------------------------------------
-- SAT Constraint Representation
-- ---------------------------------------------------------------------------

data BConstraint
  = BNeg (Term Int) (Term Int)
  | BRel Junctor (Term Int) (Term Int) (Term Int)
  | BLabel (Term Int) ID
 deriving (Eq,Show,Typeable)

data Junctor = Conjunction | Disjunction
 deriving (Eq,Show)

updateBConstr :: Store m => (Term Int -> m (Term Int)) -> BConstraint -> m BConstraint
updateBConstr update (BNeg b r) = do
  b' <- update b
  r' <- update r
  return $ BNeg b' r'
updateBConstr update (BRel junc b1 b2 r) = do
  b1' <- update b1
  b2' <- update b2
  r'  <- update r
  return $ BRel junc b1' b2' r'
updateBConstr update (BLabel b i) = do
  b' <- update b
  return $ BLabel b' i

-- ---------------------------------------------------------------------------
-- WrappableConstraint instances
-- ---------------------------------------------------------------------------

instance WrappableConstraint FDConstraint where
  updateVars = updateFDConstr updateTerm

instance WrappableConstraint BConstraint where
  updateVars = updateBConstr updateTerm
