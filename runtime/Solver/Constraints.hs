{-# LANGUAGE DeriveDataTypeable #-}

module Solver.Constraints (
                narrowIfFree, narrowIfFree2
              , ArithOp (..)
              , FDConstraint (..)
              , RelOp (..)
              , LabelingStrategy (..)
              , BConstraint (..)
              , Junctor (..)
              , updateFDConstr, updateBConstr
              , allDifferent
              ) where

import PrimTypes
import Types

import Data.Typeable

-- Converts a free variable in a narrowed variable if its cover depth is smaller than
-- the depth of its environment
-- @param contFree: continuation in case given argument is a free variable
-- @param contVal: continuation in case given argument is a value in HNF
narrowIfFree :: (NonDet a, NonDet b) => a -> (a -> Cover ->  ConstStore -> b) -> (a -> Cover -> ConstStore -> b) -> Cover -> ConstStore -> b
narrowIfFree x contFree contVal cd cs = case try x of
  (Narrowed cdi i xs) -> choicesCons cdi i (map (\x' -> narrowIfFree x' contFree contVal cd cs) xs)
  (Free cdi i xs)     -> lookupCs cs i (\xval -> contVal xval cd cs)
                           (if cdi < cd then (narrowIfFree (narrows cs cdi i id xs) contFree contVal cd cs)
                                        else contFree x cd cs)
  (Val vx)            -> contVal vx cd cs

-- Like narrowIfFree, but converts both given arguments
narrowIfFree2 :: (NonDet a, NonDet b, NonDet c) => a -> b -> (a -> b -> Cover -> ConstStore -> c) -> (a -> b -> Cover -> ConstStore -> c) -> Cover -> ConstStore -> c
narrowIfFree2 x y contFree contVal cd cs = narrowIfFree x (\x' cd' cs' -> narrowIfFree y (contFree x') (contFree x') cd' cs') (\x'' cd'' cs'' -> narrowIfFree y (contFree x'') (contVal x'') cd'' cs'') cd cs

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

-- Checks whether all elements of the given list are different
allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (v:vs) = all (v/=) vs && allDifferent vs

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
  updateVars cd = updateFDConstr (updateTerm cd)

instance WrappableConstraint BConstraint where
  updateVars cd = updateBConstr (updateTerm cd)
