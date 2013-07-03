{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module MCPSolver (GecodeSolver, OvertonSolver) where

import ExternalSolver
import FDData (FDConstraint(..), RelOp(..), ArithOp(..), LabelingStrategy(..))
import PrimTypes (C_Int)
import Types

import Control.CP.ComposableTransformers as MCP (solve)
import Control.CP.EnumTerm (assignments, labelling, inOrder, firstFail, middleOut, endsOut, EnumTerm(..))
import Control.CP.FD.FD (getColItems, FDInstance, FDSolver(..))
import Control.CP.FD.Gecode.Common (GecodeWrappedSolver)
import Control.CP.FD.Gecode.Runtime (RuntimeGecodeSolver)
import Control.CP.FD.Interface (colList)
import Control.CP.FD.Model (cte, asExpr, asCol, Model, ModelInt, ModelCol, ModelIntTerm(..), ModelColTerm(..), ModelFunctions)
import Control.CP.FD.OvertonFD.OvertonFD (OvertonFD)
import Control.CP.FD.OvertonFD.Sugar
import Control.CP.FD.Solvers (dfs, it)
import Control.CP.SearchTree (addC, Tree, MonadTree(..))
import Data.Expr.Sugar ((@=), (@/=), (@<), (@<=), (@+), (@-), (@*), (@:), (@!!), xsum, allDiff, forall, size, list, ToBoolExpr(..))


import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- ---------------------------------------------------------------------------
-- Solver Monads and ExternalSolver instances
-- ---------------------------------------------------------------------------

-- the gecode solver monad
newtype GecodeSolver a = Gecode { gecodeSolver :: State MCPState a }
 deriving (Monad, MonadState MCPState)

-- the overton solver monad
newtype OvertonSolver a = Overton { overtonSolver :: State MCPState a }
 deriving (Monad, MonadState MCPState)

instance ExternalSolver GecodeSolver where
  type ForConstraint GecodeSolver = FDConstraint
  type SolverModel   GecodeSolver = [Model]
  type Solutions     GecodeSolver = MCPSolutions

  translate    = mapM (translateMCP translateGecodeList)
  solve        = solveWithGecode
  makeBindings = makeBindingsMCP
  run          = flip evalState initial . gecodeSolver

instance ExternalSolver OvertonSolver where
  type ForConstraint OvertonSolver = FDConstraint
  type SolverModel   OvertonSolver = [Model]
  type Solutions     OvertonSolver = MCPSolutions

  translate    = mapM (translateMCP translateOvertonList)
  solve        = solveWithOverton
  makeBindings = makeBindingsMCP
  run          = flip evalState initial . overtonSolver

-- ---------------------------------------------------------------------------
-- Translation to MCP model
-- ---------------------------------------------------------------------------

-- Stores MCP representation of constraint variables
-- @key   - Integer value provided by (getKey i) (i :: ID)
-- @value - MCP representation of constraint variable with ID i 
type IntVarMap = Map.Map Integer ModelInt

-- Stores MCP representation of lists of constraint variables
-- @key   - Integer value provided by (getKey i) (i :: ID)
-- @value - MCP representation of list of constraint variables with ID i 
type ColVarMap = Map.Map Integer ModelCol

-- Translation state for Haskell's state monad
-- @intVarMap     - Table of already translated constraint variables
-- @colVarMap     - Table of already translated lists of constraint variables
--                  (only used for translateGecode)
-- @nextIntVarRef - Next variable reference
-- @nextColVarRef - Next list variable reference (only used GecodeSolver)
-- @additionalCs  - additional constraints for MCP collections 
--                  (only used for GecodeSolver)
-- @labelInfo     - labeling information
data MCPState = MCPState { 
  intVarMap     :: IntVarMap,
  colVarMap     :: ColVarMap,
  nextIntVarRef :: Int,
  nextColVarRef :: Int,
  additionalCs  :: [Model],
  labelInfo     :: MCPLabelInfo
}

-- |Type for storing labeling information for the MCP solvers:
-- @labelVars    - labeling variables in original representation
-- @domainVars   - list of fd variables, for which a domain was defined
--                 necessary to check, whether a domain was defined for
--                 the labeling variables
-- @mcpLabelVars - labeling variables translated into corresponding MCP
--                 representation
-- @labelID      - fresh ID, necessary for constructing choices over solutions,
--                 when transforming solver solutions into binding constraints
-- @strategy     - labeling strategy
data MCPLabelInfo = Info { 
--  labelVars    :: Maybe (FDList (Term Int)),
  labelVarIDs  :: [Maybe ID],
--  domainVars   :: [Term Int],
  mcpLabelVars :: Maybe ModelCol,
  labelID      :: Maybe ID,
  strategy     :: Maybe LabelingStrategy
}

-- Initial state
initial :: MCPState
initial = MCPState { 
  intVarMap     = Map.empty,
  colVarMap     = Map.empty,
  nextIntVarRef = 0,
  nextColVarRef = 0,
  additionalCs  = [],
  labelInfo     = emptyLabelInfo
}

-- Initial (empty) labeling information
emptyLabelInfo :: MCPLabelInfo
emptyLabelInfo = Info {
--  labelVars    = Nothing,
  labelVarIDs  = [],
--  domainVars   = [],
  mcpLabelVars = Nothing,
  labelID      = Nothing,
  strategy     = Nothing
}

-- getter functions for labeling information
getLabelVarIDs :: MCPLabelInfo -> [Maybe ID]
getLabelVarIDs = labelVarIDs

getMCPLabelVars :: MCPLabelInfo -> ModelCol
getMCPLabelVars = fromJust . mcpLabelVars

getLabelID :: MCPLabelInfo -> ID
getLabelID = fromJust . labelID

getStrategy :: MCPLabelInfo -> LabelingStrategy
getStrategy = fromJust . strategy

data MCPSolutions = Solutions [[C_Int]] [Maybe ID] ID

-- Translates FDConstraints to MCP constraints
-- @transList - function to translate a list of fd terms to a MCP collection
--              (Gecode- and Overton-Solver use different representations)
translateMCP :: (ExternalSolver solver, MonadState MCPState solver) 
             => (FDList (Term Int) -> solver ModelCol) -> FDConstraint 
             -> solver Model
translateMCP _ (FDRel op t1 t2) = do
  t1' <- translateTerm t1
  t2' <- translateTerm t2
  let op' = translateRelOp op
  return $ op' t1' t2'
translateMCP _ (FDArith op t1 t2 result) = do
  t1'     <- translateTerm t1
  t2'     <- translateTerm t2
  result' <- translateTerm result
  let op' = translateArithOp op
  return $ op' t1' t2' @= result'
translateMCP transList (FDSum list result) = do
  list'   <- transList list
  result' <- translateTerm result
  return $ xsum list' @= result'
translateMCP transList (FDAllDifferent list) = do
  list' <- transList list
  return $ allDiff list'
translateMCP transList (FDDomain list lower upper) = do
  list'  <- transList list
  lower' <- translateTerm lower
  upper' <- translateTerm upper
  return $ forall list' (\v -> v @: (lower',upper'))
translateMCP transList (FDLabeling strat list@(FDList _ ts) j) = do
  list' <- transList list
  state <- get
  let info = Info { labelVarIDs  = map getVarID ts
                  , mcpLabelVars = Just list'
                  , labelID      = Just j
                  , strategy     = Just strat
                  }
  put state { labelInfo = info }
  return $ toBoolExpr True

-- Translates integer terms to appropriate MCP terms using a state monad
translateTerm :: (ExternalSolver solver, MonadState MCPState solver) 
              => Term Int -> solver ModelInt
translateTerm (Const x)   = return (cte x)
translateTerm v@(Var i) = do 
  state <- get
  maybe (newVar v) return (Map.lookup (getKey i) (intVarMap state))

-- Creates a new MCP variable for the given constraint variable,
-- Updates the state by inserting the MCP representation of the variable into
-- the map and incrementing the varref counter
newVar :: (ExternalSolver solver, MonadState MCPState solver) => Term Int 
       -> solver ModelInt
newVar (Var i) = do
  state <- get
  let varMap = intVarMap state
      varRef = nextIntVarRef state
      nv     = asExpr (ModelIntVar varRef :: ModelIntTerm ModelFunctions)
  put state { nextIntVarRef = varRef + 1
            , intVarMap = Map.insert (getKey i) nv varMap
            }
  return nv

-- Translates a list of fd terms to a MCP collection for the Gecode Solver
translateGecodeList :: FDList (Term Int) -> GecodeSolver ModelCol
translateGecodeList list@(FDList i _) = do 
  state <- get
  maybe (newColVar list) return (Map.lookup (getKey i) (colVarMap state))

-- Translates a list of fd terms to a MCP collection for the Overton Solver
translateOvertonList :: FDList (Term Int) -> OvertonSolver ModelCol
translateOvertonList (FDList _ ts) = do
  mcpExprList <- mapM translateTerm ts
  return (list mcpExprList)

-- Creates a new MCP collection variable for the given list,
-- Updates the state by inserting its MCP representation into the map and
-- incrementing the corresponding varref counter,
-- Creates additional constraints for the collection variable describing its
-- size and elements (only used for translateGecode)
newColVar :: FDList (Term Int) -> GecodeSolver ModelCol
newColVar (FDList i ts) = do
  ts'   <- mapM translateTerm ts
  state <- get
  let varMap = colVarMap state
      varRef = nextColVarRef state
      nv     = asCol (ModelColVar varRef :: ModelColTerm ModelFunctions)
      colCs  = additionalCs state
  put state { nextColVarRef = varRef + 1
            , colVarMap     = Map.insert (getKey i) nv varMap
            , additionalCs  = colCs ++ (newColCs nv ts')
            }
  return nv

-- Creates additional constraints for collection variables describing the size
-- of a collection and its elements (only used for translateGecode)
newColCs :: ModelCol -> [ModelInt] -> [Model]
newColCs col ts = (size col @= cte (length ts)) : newColCs' col ts 0
 where
  newColCs' _   []     _ = []
  newColCs' col (t:ts) n = ((col @!! n) @= t) : newColCs' col ts (n+1)

-- Translates a relational operator to a corresponding MCP operator
translateRelOp Equal     = (@=)
translateRelOp Diff      = (@/=)
translateRelOp Less      = (@<)
translateRelOp LessEqual = (@<=)

-- Translates an arithmetic operator to a corresponding MCP operator
translateArithOp Plus  = (@+)
translateArithOp Minus = (@-)
translateArithOp Mult  = (@*)

-- ---------------------------------------------------------------------------
-- Solving MCP model
-- ---------------------------------------------------------------------------

type GecodeTree 
  = Tree (FDInstance (GecodeWrappedSolver RuntimeGecodeSolver)) ModelCol

type OvertonTree = Tree (FDInstance OvertonFD) ModelCol

-- Transform a list of MCP constraints into a monadic MCP model tree
toModelTree :: FDSolver s => [Model] -> ModelCol 
            -> Tree (FDInstance s) ModelCol
toModelTree model mcpLabelVars = mapM_ (\m -> addC (Left m)) model >> return mcpLabelVars

-- select corresponding MCP labeling function for given labeling strategy
matchStrategy :: EnumTerm s t => LabelingStrategy -> [t] -> s [t]
matchStrategy FirstFail = firstFail
matchStrategy MiddleOut = middleOut
matchStrategy EndsOut   = endsOut
matchStrategy _         = inOrder

-- Label MCP collection with given strategy
labelWith :: (FDSolver s, MonadTree m, TreeSolver m ~ FDInstance s, EnumTerm s (FDIntTerm s)) => LabelingStrategy -> ModelCol -> m [TermBaseType s (FDIntTerm s)]
labelWith strat col = label $ do
  lst <- getColItems col maxBound
  return $ do
    lsti <- colList col $ length lst
    labelling (matchStrategy strat) lsti
    assignments lsti
  
solveWithGecode :: [Model] -> GecodeSolver MCPSolutions
solveWithGecode model = do
  state <- get
  let info = labelInfo state
  case (labelVarIDs info) of 
    []  -> error "MCPSolver.solveWithGecode: Found no variables for labeling."
    ids -> do let addCs     = additionalCs state
                  modelTree = toModelTree (model ++ addCs) (getMCPLabelVars info)
                  solutions = snd $ MCP.solve dfs it $ 
                    (modelTree :: GecodeTree) >>= labelWith (getStrategy info)
                  cints     = map (map toCurry) solutions
              return $ Solutions cints ids (getLabelID info)

solveWithOverton :: [Model] -> OvertonSolver MCPSolutions
solveWithOverton model = do
  state <- get
  let info = labelInfo state
  case (labelVarIDs info) of
    []  -> error "MCPSolver.solveWithOverton: Found no variables for labeling."
    ids -> do let modelTree = toModelTree model (getMCPLabelVars info)
                  solutions = snd $ MCP.solve dfs it $
                    (modelTree :: OvertonTree) >>= labelWith (getStrategy info)
                  cints     = map (map toCurry) solutions
              return $ Solutions cints ids (getLabelID info)

makeBindingsMCP :: ExternalSolver solver => MCPSolutions -> solver Constraints
makeBindingsMCP (Solutions solutions ids j) = return $ StructConstr $ bindSolutions ids solutions j
