{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Solver.Overton.OvertonUtils where

import PrimTypes (C_Int)
import Solver.Constraints (FDConstraint (..), RelOp (..), ArithOp (..))
import Solver.States (fdState)
import Solver.Interface
import Solver.Overton.OvertonFD ( OvertonFD, FDVar, FDState (..), newVar, lookupDomain
                                , same, different, (.<.), (.<=.), allDifferent, addAbs
                                , addSum, addSub, addMult, addDiv, sumList, labelling, runFD, updateSolver )
import Solver.Overton.OvertonDomain (ToDomain, findMin, findMax)
import Types

import Control.Monad.State.Lazy
import qualified Data.Map as Map (lookup, insert)

-- Overton solver
type Overton = ConstraintSolver FDState

overtonSolver :: Overton
overtonSolver = Solver {
  getState      = getFDState,
  setState      = setFDState
}

instance IncrementalSolver Overton where

  type ForConstraint Overton = FDConstraint

  processWith = processFDConstr

getFDState :: Store m => m FDState
getFDState = liftM fdState getSolverStates

setFDState :: Store m => FDState -> m ()
setFDState state = do ss <- getSolverStates
                      setSolverStates ss { fdState = state }

processFDConstr :: (Store m, NonDet a) => Overton -> Cover -> FDConstraint -> a -> Solution m a
processFDConstr solver cd c@(FDLabeling vs i) val = do
  state <- getState solver
  let labelAction = mapM translateTerm vs >>= labelling
  case runFD labelAction state of
    []        -> noSolution
    solutions -> do let cints = map (map toCurry) solutions :: [[C_Int]]
                    mkSolution $ bindSolutions cd vs cints i val
processFDConstr solver _ c val = do
  state <- getState solver
  case updateSolver (addConstr c) state of
    [state'] -> do let reset = setState solver state
                   setState solver state'
                   return $ Just (reset,val)
    _        -> noSolution

-- Lookup the fd variable for the given term or create a new variable with given domain if necessary
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

-- get integer value for given term
getVal :: Term Int -> OvertonFD Int
getVal (Const c) = return c
getVal (Var _)   = error "OvertonUtils.getVal: Expected integer constant but got FDVar"
 
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

-- translate KiCS2 internal FD constraint representation
-- to constraint computation in Overton monad
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
  matchArithOp Div   = addDiv

addConstr (FDAbs x r) = do
  x' <- translateTerm x
  r' <- translateTerm r
  z  <- addAbs x'
  same r' z

addConstr (FDSum vs r) = do
  vs' <- mapM translateTerm vs
  r'  <- translateTerm r
  z   <- sumList vs'
  same r' z

addConstr (FDAllDifferent vs) = do
  vs' <- mapM translateTerm vs
  allDifferent vs'

addConstr (FDDomRange vs l u) = do
  lb <- getLB l
  ub <- getUB u
  mapM_ (lookupOrCreateFDVar (lb,ub)) vs

addConstr (FDDomSet vs dom) = do
  dom' <- mapM getVal dom
  mapM_ (lookupOrCreateFDVar dom') vs

bindSolution :: (Unifiable a, NonDet b) => Cover -> [Term Int] -> [a] -> ID -> b -> b
bindSolution cd vs solution i val = mkGuardExt cd (ValConstr i solution (concat (zipWith (bindValue cd) vs solution))) val
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
