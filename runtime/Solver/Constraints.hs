{-# LANGUAGE DeriveDataTypeable #-}

module Solver.Constraints (
                narrowIfFree, narrowIfFree2
              , ArithOp (..)
              , FDConstraint (..)
              , RelOp (..)
              , updateFDConstr
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
  | FDAbs (Term Int) (Term Int)
  | FDSum [Term Int] (Term Int)
  | FDAllDifferent [Term Int]
  | FDDomain [Term Int] (Term Int) (Term Int)
  | FDLabeling [Term Int] ID
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
updateFDConstr update (FDAbs t r) = do
  t' <- update t
  r' <- update r
  return $ FDAbs t' r'
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
updateFDConstr update (FDLabeling vs j) = do
  vs' <- mapM update vs
  return $ FDLabeling vs' j

-- ---------------------------------------------------------------------------
-- WrappableConstraint instances
-- ---------------------------------------------------------------------------

instance WrappableConstraint FDConstraint where
  updateVars cd = updateFDConstr (updateTerm cd)
