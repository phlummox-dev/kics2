{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Solver.SatSolver (SatSolver) where

import PrimTypes (C_Int)
import Solver.ExternalSolver
import Solver.Constraints (BConstraint(..), Junctor(..))
import Types

import qualified Data.Boolean.SatSolver as Sat (newSatSolver, assertTrue, lookupVar, solve, Boolean(..), SatSolver)

import Control.Monad.State (evalState, get, put, State, MonadState)
import qualified Data.Map as Map (empty, insert, lookup, Map)

-- ---------------------------------------------------------------------------
-- Solver Monad and ExternalSolver instance
-- ---------------------------------------------------------------------------

-- SAT solver monad
newtype SatSolver a = Sat { satSolver :: State SatState a }
 deriving (Monad, MonadState SatState)

instance ExternalSolver SatSolver where
  type ForConstraint SatSolver = BConstraint
  type SolverModel SatSolver = Sat.Boolean
  type Solutions SatSolver = SatSolutions

  translate    = translateSAT
  solve        = solveSAT
  makeBindings = makeBindingsSAT
  run          = flip evalState initial . satSolver

-- ---------------------------------------------------------------------------
-- Translation to formula for SAT solver written by Sebastian Fischer
-- ---------------------------------------------------------------------------

type VarMap = Map.Map Integer Sat.Boolean

-- Translation state for Haskell's state monad
-- @vars       - List of IDs of all the boolean variables used in the model
-- @varMap     - Table of already translated boolean variables
-- @nextVarRef - Next variable reference
-- @labelID    - fresh ID, necessary for constructing choices over solutions,
--               when transforming solver solutions into binding constraints
-- @satVars    - list of boolean variables which shall be tested for satisfiability
data SatState = SatState {
  vars       :: [Maybe ID],
  varMap     :: VarMap,
  nextVarRef :: Int,
  labelID    :: Maybe ID,
  satVars    :: [Sat.Boolean]
}

-- Initial state
initial :: SatState
initial = SatState {
  vars       = [],
  varMap     = Map.empty,
  nextVarRef = 0,
  labelID    = Nothing,
  satVars    = []
}

translateSAT :: [BConstraint] -> SatSolver Sat.Boolean
translateSAT cs = do
  mapM_ translateFormulas cs
  state <- get
  return $ getFormula (satVars state)

translateFormulas :: BConstraint -> SatSolver Sat.Boolean
translateFormulas (BNeg b (Var i)) = do
  b'    <- translateTerm b
  state <- get
  let boolean = Sat.Not b'
  put state { varMap = Map.insert (getKey i) boolean (varMap state) }
  return $ boolean
translateFormulas (BRel junctor b1 b2 (Var i)) = do
  b1'   <- translateTerm b1
  b2'   <- translateTerm b2
  state <- get
  let junctor' = translateJunctor junctor
      boolean  = junctor' b1' b2'
  put state { varMap = Map.insert (getKey i) boolean (varMap state) }
  return $ boolean
translateFormulas (BLabel b i) = do
  b' <- translateTerm b
  state <- get
  let svars = satVars state
  put state { labelID   = Just i
            , satVars = b' : svars
            }
  return b'

getFormula :: [Sat.Boolean] -> Sat.Boolean
getFormula [] = Sat.No
getFormula fs = foldl1 (Sat.:&&:) fs


translateJunctor :: Junctor -> Sat.Boolean -> Sat.Boolean -> Sat.Boolean
translateJunctor Conjunction = (Sat.:&&:)
translateJunctor Disjunction = (Sat.:||:)

translateTerm :: Term Int -> SatSolver Sat.Boolean
translateTerm (Const 1)   = return Sat.Yes
translateTerm (Const 0)   = return Sat.No
translateTerm t@(Const _) = error $
  "SatSolver.translateTerm: Invalid Term: " ++ show t
translateTerm v@(Var i) = do 
  state <- get
  maybe (newVar v) return (Map.lookup (getKey i) (varMap state))

newVar :: Term Int -> SatSolver Sat.Boolean
newVar (Var i) = do
  state <- get
  let m = varMap state
      varRef = nextVarRef state
      nv     = Sat.Var varRef
  put state { nextVarRef = varRef + 1
            , varMap     = Map.insert (getKey i) nv m
            , vars       = (Just i) : (vars state)
            }
  return nv

-- ---------------------------------------------------------------------------
-- Solving formula with SAT solver written by Sebastian Fischer
-- ---------------------------------------------------------------------------

data SatSolutions = Solutions [[C_Int]] [Maybe ID] ID

solveSAT :: Sat.Boolean -> SatSolver SatSolutions
solveSAT formula = do
  state <- get
  case (labelID state) of
    Nothing -> error "Usage: To test the satisfiability of a formula: CLPB> satisfied <formula>."
    Just i  -> do
      let initSolver = Sat.assertTrue formula Sat.newSatSolver >>= Sat.solve
          varRefs = [0..((nextVarRef state) - 1)]
          solutions = map (\s -> map (flip Sat.lookupVar s) varRefs) initSolver
          ints = concatMap boolsAsInts solutions
          cints = map (map toCurry) ints
      return $ Solutions cints (vars state) i

makeBindingsSAT :: ExternalSolver solver => Cover -> SatSolutions -> solver Constraints
makeBindingsSAT cd (Solutions solutions ids j) = return $ StructConstr $ bindSolutions cd ids solutions j

boolAsInt :: Bool -> Int
boolAsInt False = 0
boolAsInt True  = 1

boolsAsInts :: [Maybe Bool] -> [[Int]]
boolsAsInts [] = [[]]
boolsAsInts (b:bs) = case b of
  Nothing -> map (0:) rest ++ map (1:) rest
  Just x  -> let i = boolAsInt x in map (i:) rest
 where rest = boolsAsInts bs
