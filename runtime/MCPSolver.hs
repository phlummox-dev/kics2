{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MCPSolver where

import ExternalSolver
import Types
import FDData
import PrimTypes

import Data.Expr.Sugar
import Control.CP.ComposableTransformers (solve)
import Control.CP.SearchTree (addC, Tree (..),MonadTree(..))
import Control.CP.EnumTerm
import Control.CP.FD.Model
import Control.CP.FD.FD (FDInstance, FDSolver(..), getColItems)
import Control.CP.FD.Interface (colList)
import Control.CP.FD.Solvers
import Control.CP.FD.Gecode.Common (GecodeWrappedSolver)
import Control.CP.FD.Gecode.Runtime (RuntimeGecodeSolver)
import Control.CP.FD.OvertonFD.OvertonFD
import Control.CP.FD.OvertonFD.Sugar

import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List ((\\))

-- ---------------------------------------------------------------------------
-- ExternalFDSolver instance for MCP Solvers
-- ---------------------------------------------------------------------------

instance ExternalFDSolver MCPSolver where

  type ForConstraint MCPSolver = FDConstraint

  type SolverModel MCPSolver = ([Model], MCPLabelInfo)

  type Solutions MCPSolver = MCPSolutions

  translate Overton fdCs = translateOverton fdCs
  translate Gecode  fdCs = translateGecode fdCs

  solveWith = solveWithMCP 

  makeBindings _ (Solutions solutions ids i) = StructConstr $ bindSolutions ids solutions i


-- type synonyms for easier access to associated types
type MCPModel = SolverModel MCPSolver

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
-- @colVarMap     - Table of already translated lists of constraint variables (only used for translateGecode)
-- @nextIntVarRef - Next variable reference
-- @nextColVarRef - Next list variable reference (only used for translateGecode)
-- @additionalCs  - additional constraints for MCP collections (only used for translateGecode)
-- @labelInfo     - collected labeling information
data TLState = TLState { 
  intVarMap     :: IntVarMap,
  colVarMap     :: ColVarMap,
  nextIntVarRef :: Int,
  nextColVarRef :: Int,
  additionalCs  :: [Model],
  labelInfo     :: MCPLabelInfo
}

-- Initial state
baseTLState :: TLState
baseTLState = TLState { 
  intVarMap     = Map.empty,
  colVarMap     = Map.empty,
  nextIntVarRef = 0,
  nextColVarRef = 0,
  additionalCs  = [],
  labelInfo     = baseLabelInfo
}

-- Initial (empty) labeling information
baseLabelInfo :: MCPLabelInfo
baseLabelInfo = Info {
  labelVars    = Nothing,
  labelVarIDs  = Nothing,
  domainVars   = [],
  mcpLabelVars = Nothing,
  labelID      = Nothing,
  strategy     = Nothing
}

-- |Type for storing labeling information for the MCP solvers:
-- @labelVars    - labeling variables in original representation
-- @domainVars   - list of fd variables, for which a domain was defined
--                 necessary to check, whether a domain was defined for the labeling variables
-- @mcpLabelVars - labeling variables translated into corresponding MCP representation
-- @labelID      - fresh ID, necessary for constructing choices over solutions, when transforming
--                 solver solutions into binding constraints
-- @strategy     - labeling strategy
data MCPLabelInfo = Info { 
  labelVars    :: Maybe (FDList (FDTerm Int)),
  labelVarIDs  :: Maybe [Maybe ID],
  domainVars   :: [FDTerm Int],
  mcpLabelVars :: Maybe ModelCol,
  labelID      :: Maybe ID,
  strategy     :: Maybe LabelingStrategy
}

data MCPSolutions = Solutions [[C_Int]] [Maybe ID] ID

-- The Overton and Gecode solvers work on different representations of lists of constraint variables.
-- Therefore each solver has its own translation function:

-- Translates list of finite domain constraints into a MCP model for the Overton Solver
-- and collects labeling information if available
-- using Haskell's state monad
translateOverton :: [FDConstraint] -> MCPModel
translateOverton fdCs = let (mcpCs,state) = runState (mapM (translateConstr translateOvertonList) fdCs) baseTLState
                        in (mcpCs, labelInfo state)

-- Translates list of finite domain constraints into a MCP model for the Gecode Solver
-- and collects labeling information if available
-- using Haskell's state monad
translateGecode :: [FDConstraint] -> MCPModel
translateGecode fdCs = let (mcpCs,state) = runState (mapM (translateConstr translateGecodeList) fdCs) baseTLState
                       in ((mcpCs ++ (additionalCs state)), labelInfo state)


-- Translates a single finite domain constraint into a specific MCP constraint
-- using Haskell's state monad.
-- This function works for both solvers by calling different functions to translate lists
-- @tlList - function to translate lists of constraint variables to MCP collections
translateConstr :: (FDList (FDTerm Int) -> State TLState ModelCol) -> FDConstraint -> State TLState Model
translateConstr _ (FDRel relop t1 t2)       = do mcpTerm1 <- translateTerm t1
                                                 mcpTerm2 <- translateTerm t2
                                                 let mcpRelop = translateRelOp relop
                                                 return $ mcpRelop mcpTerm1 mcpTerm2
translateConstr _ (FDArith arithOp t1 t2 r) = do mcpTerm1  <- translateTerm t1
                                                 mcpTerm2  <- translateTerm t2
                                                 mcpResult <- translateTerm r
                                                 let mcpArithOp = translateArithOp arithOp
                                                 return $ (mcpArithOp mcpTerm1 mcpTerm2) @= mcpResult
translateConstr tlList (FDSum vs r)         = do mcpVs     <- tlList vs
                                                 mcpResult <- translateTerm r
                                                 return $ (xsum mcpVs) @= mcpResult
translateConstr tlList (FDAllDifferent vs)  = do mcpVs <- tlList vs
                                                 return $ allDiff mcpVs
translateConstr tlList (FDDomain vs@(FDList _ ts) l u) = do mcpVs <- tlList vs
                                                            mcpL  <- translateTerm l
                                                            mcpU  <- translateTerm u
                                                            state <- get
                                                            let info     = labelInfo state
                                                                dVars    = domainVars info
                                                                newInfo  = info { domainVars = dVars ++ ts }
                                                                newState = state { labelInfo = newInfo }
                                                 --let domain varList lower upper = forall varList (\var -> var @: (lower,upper))
                                                            put newState
                                                            return $ domain mcpVs mcpL mcpU
translateConstr tlList (FDLabeling str vs@(FDList _ list) j) = do mcpVs <- tlList vs
                                                                  state <- get
                                                                  let ids      = map getVarID list
                                                                      info     = labelInfo state
                                                                      newInfo  = info { labelVars    = Just vs
                                                                                      , labelVarIDs  = Just ids
                                                                                      , mcpLabelVars = Just mcpVs
                                                                                      , labelID      = Just j
                                                                                      , strategy     = Just str
                                                                                      }
                                                                      newState = state { labelInfo = newInfo }
                                                                  put newState 
                                                                  return (toBoolExpr True)

-- Constraining a MCP collection of variables to a domain
-- defined by a lower and upper boundary
domain :: ModelCol -> ModelInt -> ModelInt -> Model
domain varList lower upper = forall varList (\var -> var @: (lower,upper))

-- Translates integer terms to appropriate MCP terms
-- using Haskell's state monad
translateTerm :: FDTerm Int -> State TLState ModelInt
translateTerm (Const x) = return (cte x)
translateTerm v@(FDVar i) = do state <- get
                               let varMap = intVarMap state
                               maybe (newVar v) return (Map.lookup (getKey i) varMap)

-- Creates a new MCP variable for the given constraint variable
-- Updates the translation state by inserting the MCP representation
-- of the variable into the map and incrementing the varref counter
newVar :: FDTerm Int -> State TLState ModelInt
newVar (FDVar i) = do state <- get
                      let varMap   = intVarMap state
                          varRef   = nextIntVarRef state
                          nvar     = asExpr (ModelIntVar varRef :: ModelIntTerm ModelFunctions)
                          newState = state { nextIntVarRef = varRef + 1
                                           , intVarMap = Map.insert (getKey i) nvar varMap
                                           }
                      put newState
                      return nvar

-- Translates lists of fd terms to MCP collection for the Overton Solver
translateOvertonList :: FDList (FDTerm Int) -> State TLState ModelCol
translateOvertonList (FDList _ vs) = do mcpExprList <- mapM translateTerm vs
                                        return (list mcpExprList)

-- Translates lists of fd terms to MCP collection for the Gecode Solver
translateGecodeList :: FDList (FDTerm Int) -> State TLState ModelCol
translateGecodeList l@(FDList i vs) = do state <- get
                                         let varMap = colVarMap state
                                         maybe (newColVar l) return (Map.lookup (getKey i) varMap)

-- Creates a new MCP collection variable for the given list,
-- Updates the translation state by inserting its MCP representation
-- into the map and incrementing the corresponding varref counter,
-- Creates additional constraints for the collection variable describing its size and elements
-- (only used for translateGecode)
newColVar :: FDList (FDTerm Int) -> State TLState ModelCol
newColVar (FDList i vs) = do mcpVs <- mapM translateTerm vs
                             state <- get
                             let varMap   = colVarMap state
                                 varRef   = nextColVarRef state
                                 nvar     = asCol (ModelColVar varRef :: ModelColTerm ModelFunctions)
                                 colCs    = additionalCs state
                                 newState = state { nextColVarRef = varRef + 1
                                                  , colVarMap = Map.insert (getKey i) nvar varMap
                                                  , additionalCs = colCs ++ (newColCs nvar mcpVs)
                                                  }
                             put newState
                             return nvar

-- Creates additional constraints for collection variables 
-- describing the size of a collection and its elements 
-- (only used for translateGecode)newColCs :: ModelCol -> [ModelInt] -> [Model]
newColCs col vs = (size col @= cte (length vs)) : newColCs' col vs 0
  where
   newColCs' _   []     _ = []
   newColCs' col (v:vs) n = ((col @!! n) @= v) : newColCs' col vs (n+1) 

-- Translates relational operators to appropriate MCP operators
translateRelOp Equal     = (@=)
translateRelOp Diff      = (@/=)
translateRelOp Less      = (@<)
translateRelOp LessEqual = (@<=)

-- Translates arithmetic operators to appropriate MCP operators
translateArithOp Plus  = (@+)
translateArithOp Minus = (@-)
translateArithOp Mult  = (@*)

-- ---------------------------------------------------------------------------
-- Solving MCP model
-- ---------------------------------------------------------------------------

-- Types for MCP model trees parametrized with specific FD solver:
type OvertonTree = Tree (FDInstance OvertonFD) ModelCol
type GecodeTree  = Tree (FDInstance (GecodeWrappedSolver RuntimeGecodeSolver)) ModelCol

-- Calls solve function for specific solver
solveWithMCP :: MCPSolver -> MCPModel -> MCPSolutions
solveWithMCP Overton (mcpCs,info) = solveWithOverton mcpCs info
solveWithMCP Gecode  (mcpCs,info) = solveWithGecode  mcpCs info

-- Calls Overton Solver with corresponding model tree and prepare solutions for KiCS2
solveWithOverton :: [Model] -> MCPLabelInfo -> MCPSolutions
solveWithOverton mcpCs info = case maybeLabelVars of 
  Nothing    -> error "MCPSolver.solveWithOverton: Found no variables for labeling."
  Just lVars -> 
    if (not (inDomain lVars dVars)) 
      then error "MCPSolver.solveWithOverton: At least for one Labeling variable no domain was specified."
      else let mcpVars   = fromJust (mcpLabelVars info)
               ids       = fromJust (labelVarIDs info)
               choiceID  = fromJust (labelID info)
               strtgy    = fromJust (strategy info)
               modelTree = toModelTree mcpCs mcpVars
               solutions = snd $ solve dfs it $
                 (modelTree :: OvertonTree) >>= labelWith strtgy
           in (Solutions (map (map toCurry) solutions) ids choiceID)
  where maybeLabelVars = labelVars info
        dVars          = domainVars info

-- Calls Gecode Solver with corresponding model tree and prepare solutions for KiCS2
solveWithGecode :: [Model] -> MCPLabelInfo -> MCPSolutions
solveWithGecode mcpCs info = case maybeLabelVars of 
  Nothing    -> error "MCPSolver.solveWithGecode: Found no variables for labeling."
  Just lVars -> 
    if (not (inDomain lVars dVars)) 
      then error "MCPSolver.solveWithGecode: At least for one Labeling variable no domain was specified."
      else let mcpVars   = fromJust (mcpLabelVars info)
               ids       = fromJust (labelVarIDs info)
               choiceID  = fromJust (labelID info)
               strtgy    = fromJust (strategy info)
               modelTree = toModelTree mcpCs mcpVars
               solutions = snd $ solve dfs it $
                 (modelTree :: GecodeTree) >>= labelWith strtgy
           in (Solutions (map (map toCurry) solutions) ids choiceID)
  where maybeLabelVars = labelVars info
        dVars          = domainVars info

-- checks, if a domain was specified for every labeling variable
inDomain :: FDList (FDTerm Int) -> [FDTerm Int] -> Bool
inDomain (FDList _ lVars) dVars = null $ lVars \\ dVars

-- Label MCP collection with given strategy
labelWith :: (FDSolver s, MonadTree m, TreeSolver m ~ FDInstance s, EnumTerm s (FDIntTerm s)) => LabelingStrategy -> ModelCol -> m [TermBaseType s (FDIntTerm s)]
labelWith strategy col = label $ do
  lst <- getColItems col maxBound
  return $ do
    lsti <- colList col $ length lst
    labelling (matchStrategy strategy) lsti
    assignments lsti

-- select corresponding MCP labeling function for given labeling strategy
matchStrategy :: EnumTerm s t => LabelingStrategy -> [t] -> s [t]
matchStrategy FirstFail = firstFail
matchStrategy MiddleOut = middleOut
matchStrategy EndsOut   = endsOut
matchStrategy _         = inOrder

-- Transform a list of MCP constraints into a monadic MCP model tree
toModelTree :: FDSolver s => [Model] -> ModelCol -> Tree (FDInstance s) ModelCol
toModelTree model mcpLabelVars = mapM_ (\m -> addC (Left m)) model >> return mcpLabelVars
