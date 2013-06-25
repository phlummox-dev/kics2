{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module SatSolver (SatSolver) where

import ExternalSolver
import FDData (BConstraint(..), Junctor(..))
import PrimTypes (C_Int)
import Types

import qualified Data.Boolean.SatSolver as Sat (newSatSolver, assertTrue, lookupVar, solve, Boolean(..), SatSolver)

import Control.Monad.State (evalState, get, put, State, MonadState)
import qualified Data.Map as Map (empty, insert, lookup, Map)
import Data.Maybe (catMaybes)

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
data SatState = SatState {
  vars       :: [Maybe ID],
  varMap     :: VarMap,
  nextVarRef :: Int,
  labelID    :: Maybe ID
}

-- Initial state
initial :: SatState
initial = SatState {
  vars       = [],
  varMap     = Map.empty,
  nextVarRef = 0,
  labelID    = Nothing
}

translateSAT :: [BConstraint] -> SatSolver Sat.Boolean
translateSAT [] = return Sat.No
translateSAT [BLabel b i] = do
  state <- get
  put state { labelID = Just i }
  translateTerm b
translateSAT (f:fs) = do
  (boolean,i) <- translateFormula f
  state <- get
  put state { varMap = Map.insert (getKey i) boolean (varMap state) }
  translateSAT fs

translateFormula :: BConstraint -> SatSolver (Sat.Boolean,ID)
translateFormula (BNeg b (FDVar i)) = do
  b' <- translateTerm b
  return (Sat.Not b',i)
translateFormula (BRel junctor b1 b2 (FDVar i)) = do
  b1' <- translateTerm b1
  b2' <- translateTerm b2
  let junctor' = translateJunctor junctor
      boolean  = junctor' b1' b2'
  return (boolean,i)
    
{-
translateSAT :: BConstraint -> SatSolver Sat.Boolean
translateSAT (BNeg b (FDVar i)) = do
  b'    <- translateTerm b
  state <- get
  let boolean = Sat.Not b'
  put state { varMap = Map.insert (getKey i) boolean (varMap state) }
  return $ boolean
translateSAT (BRel junctor b1 b2 (FDVar i)) = do
  b1'   <- translateTerm b1
  b2'   <- translateTerm b2
  state <- get
  let junctor' = translateJunctor junctor
      boolean  = junctor' b1' b2'
  put state { varMap = Map.insert (getKey i) boolean (varMap state) }
  return $ boolean
translateSAT (BLabel b i) = do
  b' <- translateTerm b
  state <- get
  put state { labelID = Just i }
  return b'
-}

translateJunctor :: Junctor -> Sat.Boolean -> Sat.Boolean -> Sat.Boolean
translateJunctor Conjunction = (Sat.:&&:)
translateJunctor Disjunction = (Sat.:||:)

translateTerm :: FDTerm Int -> SatSolver Sat.Boolean
translateTerm (Const 1)   = return Sat.Yes
translateTerm (Const 0)   = return Sat.No
translateTerm t@(Const _) = error $
  "SatSolver.translateTerm: Invalid Term: " ++ show t
translateTerm v@(FDVar i) = do 
  state <- get
  maybe (newVar v) return (Map.lookup (getKey i) (varMap state))

newVar :: FDTerm Int -> SatSolver Sat.Boolean
newVar (FDVar i) = do
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

-- TODO: improve code
solveSAT :: Sat.Boolean -> SatSolver SatSolutions
solveSAT formula = do
  state <- get
  case (labelID state) of
    Nothing -> error "Usage: To test the satisfiability of a formula: CLPB> satisfied <formula>."
    Just i  -> 
      do let initSolver = Sat.assertTrue formula Sat.newSatSolver >>= Sat.solve
             ids = (vars state)
             bvars = catMaybes $ map (flip Map.lookup (varMap state) . getKey) (catMaybes ids)
             solutions = map (\s -> map (flip lookupBooleanVar s) bvars) initSolver
             ints = concatMap boolsAsInts solutions
             cints = map (map toCurry) ints
         return $ Solutions cints ids i

makeBindingsSAT :: ExternalSolver solver => SatSolutions -> solver Constraints
makeBindingsSAT (Solutions solutions ids j) = return $ StructConstr $ bindSolutions ids solutions j

lookupBooleanVar :: Sat.Boolean -> Sat.SatSolver -> Maybe Bool
lookupBooleanVar (Sat.Var i) = Sat.lookupVar i
lookupBooleanVar b = error $ "SatSolver.lookupBooleanVar: Given argument is not a boolean variable: " ++ (show b)

boolAsInt :: Bool -> Int
boolAsInt False = 0
boolAsInt True  = 1

boolsAsInts :: [Maybe Bool] -> [[Int]]
boolsAsInts [] = [[]]
boolsAsInts (b:bs) = case b of
  Nothing -> map (0:) rest ++ map (1:) rest
  Just x  -> let i = boolAsInt x in map (i:) rest
 where rest = boolsAsInts bs
