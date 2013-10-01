{-# LANGUAGE TypeFamilies #-}

module Solver.OvertonUtils where

import PrimTypes (C_Int)
import Solver.Constraints (FDConstraint (..), RelOp (..), ArithOp (..))
import Solver.EquationSolver (Solution, noSolution, mkSolution)
import Solver.OvertonFD ( OvertonFD, FDVar, FDState (..), newVar, lookupDomain
                        , same, different, (.<.), (.<=.), allDifferent
                        , addSum, addSub, addMult, sumList, labelling, runFD, updateSolver )
import Solver.OvertonDomain (ToDomain, findMin, findMax)
import Types

import Control.Monad.State.Lazy
import qualified Data.Map as Map (lookup, insert)

processConstr :: (Store m, NonDet a) => Cover -> FDConstraint -> a -> Solution m a
processConstr cd (FDLabeling vs i) val = do
  state <- getFDState
  let labelAction = mapM translateTerm vs >>= labelling
  case runFD labelAction state of
    []        -> noSolution
    solutions -> do let cints = map (map toCurry) solutions :: [[C_Int]]
                    mkSolution $ bindSolutions cd vs cints i val
processConstr _ c val = do
  state <- getFDState
  case updateSolver (addConstr c) state of
    [state'] -> do let reset = setFDState state
                   setFDState state'
                   return $ Just (reset,val)
    _        -> noSolution

-- Lookup the fd variable for the given term or create a new variable if necessary
lookupOrCreateFDVar :: ToDomain a => a -> Term Int -> OvertonFD FDVar
lookupOrCreateFDVar _   (Const c) = newVar c
lookupOrCreateFDVar dom (Var i)   = do
  s <- get
  maybe newFDVar return (Map.lookup k (curryVarMap s))
 where
  k        = getKey i
  newFDVar = do
    v <- newVar dom
    s <- get
    let cvm = curryVarMap s
    modify $ \s -> s { curryVarMap = Map.insert k v cvm }
    return v

translateTerm :: Term Int -> OvertonFD FDVar
translateTerm = lookupOrCreateFDVar ()
 
-- get lower boundary for given term
getLB :: Term Int -> OvertonFD Int
getLB v@(Var i) = do
  dom <- lookupOrCreateFDVar () v >>= lookupDomain
  return $ findMin dom
getLB (Const c) = return c

-- get upper boundary for given term
getUB :: Term Int -> OvertonFD Int
getUB v@(Var i) = do
  dom <- lookupOrCreateFDVar () v >>= lookupDomain
  return $ findMax dom
getUB (Const c) = return c


addConstr :: FDConstraint -> OvertonFD ()
addConstr (FDRel op x y) = do
  x' <- translateTerm x
  y' <- translateTerm y
  let constraint = matchRelOp op
  constraint x' y'
 where
  matchRelOp Equal     = same
  matchRelOp Diff      = different
  matchRelOp Less      = (.<.)
  matchRelOp LessEqual = (.<=.)

addConstr (FDArith op x y r) = do
  x' <- translateTerm x
  y' <- translateTerm y
  r' <- translateTerm r
  let constraint = matchArithOp op
  z <- constraint x' y'
  same r' z
 where
  matchArithOp Plus  = addSum
  matchArithOp Minus = addSub
  matchArithOp Mult  = addMult

addConstr (FDSum vs r) = do
  vs' <- mapM translateTerm vs
  r'  <- translateTerm r
  z   <- sumList vs'
  same r' z

addConstr (FDAllDifferent vs) = do
  vs' <- mapM translateTerm vs
  allDifferent vs'

addConstr (FDDomain vs l u) = do
  lb <- getLB l
  ub <- getUB u
  mapM_ (lookupOrCreateFDVar (lb,ub)) vs

bindSolution :: (Unifiable a, NonDet b) => Cover -> [Term Int] -> [a] -> ID -> b -> b
bindSolution cd vs solution i val = guardCons cd (ValConstr i solution (concat (zipWith (bindValue cd) vs solution))) val
 where
  bindValue :: Unifiable a => Cover -> Term Int -> a -> [Constraint]
  bindValue cd (Var i)   val = bind cd i val
  bindValue _  (Const _) _   = []

bindSolutions :: (Unifiable a, NonDet b) => Cover -> [Term Int] -> [[a]] -> ID -> b -> b
bindSolutions cd vs [solution] i val = bindSolution cd vs solution i val
bindSolutions cd vs (solution:solutions) i val = choiceCons cd i binding bindings
 where
  binding  = bindSolution cd vs solution (leftID i) val
  bindings = bindSolutions cd vs solutions (rightID i) val
