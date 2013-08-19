{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Solver.MCPUtils
  ( -- Getter for MCPLabelInfo
    getLabelID
  , getLabelVarIDs
  , getMCPLabelVars
  , getStrategy
  , isNotLabelled
    -- state for translating MCP models
  , MCPState(..), initial
  , translateMCP, translateTerm
  , labelWith, toModelTree, MCPSolutions(..)
  , makeBindingsMCP
  ) where

import PrimTypes (C_Int)
import Solver.Constraints (FDConstraint(..), RelOp(..), ArithOp(..), LabelingStrategy(..))
import Solver.ExternalSolver
import Types

import Control.CP.EnumTerm (assignments, labelling, inOrder, firstFail, middleOut, endsOut, EnumTerm(..))
import Control.CP.FD.FD (getColItems, FDInstance, FDSolver(..))
import Control.CP.FD.Interface (colList)
import Control.CP.FD.Model (cte, asExpr, asCol, Model, ModelInt, ModelCol, ModelIntTerm(..), ModelColTerm(..), ModelFunctions)
import Control.CP.SearchTree (addC, Tree, MonadTree(..))
import Data.Expr.Sugar ((@=), (@/=), (@<), (@<=), (@+), (@-), (@*), (@:), xsum, allDiff, forall, list, ToBoolExpr(..))

import Control.Monad.State
import Data.List ((\\))
import qualified Data.Map as Map
import Data.Maybe (fromJust, catMaybes)

-- ---------------------------------------------------------------------------
-- Translation to MCP model
-- ---------------------------------------------------------------------------

-- Stores MCP representation of constraint variables
-- @key   - Integer value provided by (getKey i) (i :: ID)
-- @value - MCP representation of constraint variable with ID i 
type IntVarMap = Map.Map Integer ModelInt

-- Translation state for Haskell's state monad
-- @intVarMap     - Table of already translated constraint variables
-- @colVarMap     - Table of already translated lists of constraint variables
--                  (only used for translateGecode)
-- @nextIntVarRef - Next variable reference
-- @labelInfo     - labeling information
data MCPState = MCPState { 
  intVarMap     :: IntVarMap,
  nextIntVarRef :: Int,
  labelInfo     :: MCPLabelInfo
}

-- |Type for storing labeling information for the MCP solvers:
-- @labelVarIDs  - IDs of the labeling variables
-- @domainVarIDs - IDs of the fd variables, for which a domain was defined
--                 necessary to check, whether a domain was defined for
--                 all labeling variables
-- @mcpLabelVars - labeling variables translated into corresponding MCP
--                 representation
-- @labelID      - fresh ID, necessary for constructing choices over solutions,
--                 when transforming solver solutions into binding constraints
-- @strategy     - labeling strategy
-- @notLabelled  - flag that shows whether a labeling constraint was found
--                 during the translation of the model
data MCPLabelInfo = Info { 
  labelVarIDs  :: [Maybe ID],
  domainVarIDs :: [Maybe ID],
  mcpLabelVars :: Maybe ModelCol,
  labelID      :: Maybe ID,
  strategy     :: Maybe LabelingStrategy,
  notLabelled  :: Bool
}

-- Initial state
initial :: MCPState
initial = MCPState { 
  intVarMap     = Map.empty,
  nextIntVarRef = 0,
  labelInfo     = emptyLabelInfo
}

-- Initial (empty) labeling information
emptyLabelInfo :: MCPLabelInfo
emptyLabelInfo = Info {
  labelVarIDs  = [],
  domainVarIDs = [],
  mcpLabelVars = Nothing,
  labelID      = Nothing,
  strategy     = Nothing,
  notLabelled  = True
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

isNotLabelled :: MCPLabelInfo -> Bool
isNotLabelled = notLabelled

data MCPSolutions = Solutions [[C_Int]] [Maybe ID] ID

-- Translates FDConstraints to MCP constraints
translateMCP :: (ExternalSolver solver, MonadState MCPState solver) 
             => FDConstraint -> solver Model
translateMCP (FDRel op t1 t2) = do
  t1' <- translateTerm t1
  t2' <- translateTerm t2
  let op' = translateRelOp op
  return $ op' t1' t2'
translateMCP (FDArith op t1 t2 result) = do
  t1'     <- translateTerm t1
  t2'     <- translateTerm t2
  result' <- translateTerm result
  let op' = translateArithOp op
  return $ op' t1' t2' @= result'
translateMCP (FDSum list result) = do
  list'   <- translateList list
  result' <- translateTerm result
  return $ xsum list' @= result'
translateMCP (FDAllDifferent list) = do
  list' <- translateList list
  return $ allDiff list'
translateMCP (FDDomain list lower upper) = do
  state  <- get
  let info   = labelInfo state 
      domIDs = domainVarIDs info
  put state { labelInfo = info { domainVarIDs = domIDs ++ (map getVarID list) } }
  list'  <- translateList list
  lower' <- translateTerm lower
  upper' <- translateTerm upper
  return $ forall list' (\v -> v @: (lower',upper'))
translateMCP (FDLabeling strat list j) = do
  list' <- translateList list
  state <- get
  let domIDs      = domainVarIDs (labelInfo state)
      labelIDs    = map getVarID list
      allInDomain = null $ (catMaybes labelIDs) \\ (catMaybes domIDs)
      info = Info { labelVarIDs  = labelIDs
                  , domainVarIDs = domIDs
                  , mcpLabelVars = Just list'
                  , labelID      = Just j
                  , strategy     = Just strat
                  , notLabelled  = False
                  }
  put state { labelInfo = info }
  if allInDomain then return $ toBoolExpr True
                 else error "MCPUtils.translateMCP: At least for one labeling variable no domain was specified."

-- Translates integer terms to appropriate MCP terms using a state monad
translateTerm :: (ExternalSolver solver, MonadState MCPState solver) 
              => Term Int -> solver ModelInt
translateTerm (Const x) = return (cte x)
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

-- Translates list of fd terms to a MCP collection
translateList :: (ExternalSolver solver, MonadState MCPState solver)
              => [Term Int] -> solver ModelCol
translateList vs = do
  vs' <- mapM translateTerm vs
  return $ list vs'

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

-- Transform a list of MCP constraints into a monadic MCP model tree
toModelTree :: FDSolver s => [Model] -> ModelCol 
            -> Tree (FDInstance s) ModelCol
toModelTree model mcpLabelVars = 
  mapM_ (\m -> addC (Left m)) model >> return mcpLabelVars

-- select corresponding MCP labeling function for given labeling strategy
matchStrategy :: EnumTerm s t => LabelingStrategy -> [t] -> s [t]
matchStrategy FirstFail = firstFail
matchStrategy MiddleOut = middleOut
matchStrategy EndsOut   = endsOut
matchStrategy _         = inOrder

-- Label MCP collection with given strategy
labelWith :: (FDSolver s, MonadTree m, TreeSolver m ~ FDInstance s, 
              EnumTerm s (FDIntTerm s)) => LabelingStrategy -> ModelCol 
                                        -> m [TermBaseType s (FDIntTerm s)]
labelWith strat col = label $ do
  lst <- getColItems col maxBound
  return $ do
    lsti <- colList col $ length lst
    labelling (matchStrategy strat) lsti
    assignments lsti

-- ---------------------------------------------------------------------------
-- Binding solutions
-- ---------------------------------------------------------------------------

makeBindingsMCP :: ExternalSolver solver => MCPSolutions -> solver Constraints
makeBindingsMCP (Solutions solutions ids j) =
  return $ StructConstr $ bindSolutions ids solutions j
