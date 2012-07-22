{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MCPSolver where

import Types
import FDData
import qualified Curry_Prelude as CP

--import Data.Expr.Data hiding (Const,Plus,Minus,Mult)
import Data.Expr.Sugar
import Control.CP.ComposableTransformers (solve)
import Control.CP.SearchTree (addC, Tree (..),MonadTree(..))
import Control.CP.EnumTerm
import Control.CP.FD.Model
import Control.CP.FD.FD (FDInstance, FDSolver, getColItems)
import Control.CP.FD.Interface (colList) -- added for new label
import Control.CP.FD.Solvers
import Control.CP.FD.Gecode.Common (GecodeWrappedSolver)
import Control.CP.FD.Gecode.Runtime (RuntimeGecodeSolver)
import Control.CP.FD.OvertonFD.OvertonFD
import Control.CP.FD.OvertonFD.Sugar

import qualified Data.Map as Map
import Control.Monad.State

import Debug.Trace
import Data.Maybe (fromJust)

import ExternalSolver

-- ---------------------------------------------------------------------------
-- ExternalConstraint instance for FDConstraint
-- ---------------------------------------------------------------------------
instance ExternalConstraint FDConstraint where
  updateVars = updateFDConstr updateFDVar

-- ---------------------------------------------------------------------------
-- ExternalSolver instance for MCP Solvers
-- ---------------------------------------------------------------------------


instance ExternalSolver MCPSolver FDConstraint where
  newtype SolverModel MCPSolver FDConstraint = SM [Model]

  -- |Type for storing labeling information for the MCP solvers:
  -- @labelVars    - labeling variables in original representation
  -- @mcpLabelVars - labeling variables translated into corresponding MCP representation
  --                 solver solutions into binding constraints
  -- @strategy     - labeling strategy
  data LabelInfo MCPSolver FDConstraint = Info { labelVars    :: Maybe (FDList (FDTerm Int))
                                               , mcpLabelVars :: Maybe ModelCol
                                               , labelID      :: Maybe ID
                                               , strategy     :: Maybe LabelingStrategy
                                               }
  newtype Solutions MCPSolver FDConstraint = S (SolutionInfo CP.C_Int (FDTerm Int))

  translate Overton fdCs = translateOverton fdCs
  translate Gecode  fdCs = translateGecode fdCs

  solveWith = solveWithMCP 

  makeConstrSolutions _ (S solutions) e = bindSolutions solutions e

type MCPModel = SolverModel MCPSolver FDConstraint
type MCPLabelInfo = LabelInfo MCPSolver FDConstraint
type MCPSolution = Solutions MCPSolver FDConstraint

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
  nextColVarRef :: Int,
  nextIntVarRef :: Int,
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
  mcpLabelVars = Nothing,
  labelID      = Nothing,
  strategy     = Nothing
}

-- The Overton and Gecode solvers work on different representations of lists of constraint variables.
-- Therefore each solver has its own translation function:

-- Translates list of finite domain constraints into a MCP model for the Overton Solver
-- and collects labeling information if available
-- using Haskell's state monad
translateOverton :: [FDConstraint] -> (MCPModel,MCPLabelInfo)
translateOverton fdCs = trace ("\nOverton FDConstraints:\n" ++ show fdCs ++ "\n") $
                        let (mcpCs,state) = runState (mapM (translateConstr translateOvertonList) fdCs) baseTLState
                            info = labelInfo state
                        in (SM mcpCs, info)

-- Translates list of finite domain constraints into a MCP model for the Gecode Solver
-- and collects labeling information if available
-- using Haskell's state monad
translateGecode :: [FDConstraint] -> (MCPModel,MCPLabelInfo)
translateGecode fdCs = trace ("\nGecode FDConstraints:\n" ++ show fdCs ++ "\n") $
                       let (mcpCs,state) = runState (mapM (translateConstr translateGecodeList) fdCs) baseTLState
                           info = labelInfo state
                           mcpColCs = additionalCs state
                       in (SM (mcpCs ++ mcpColCs), info)


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
translateConstr tlList (FDDomain vs l u)    = do mcpVs <- tlList vs
                                                 mcpL  <- translateTerm l
                                                 mcpU  <- translateTerm u
                                                 let domain varList lower upper = forall varList (\var -> var @: (lower,upper))
                                                 return $ domain mcpVs mcpL mcpU
translateConstr tlList (FDLabeling str vs j) = do state <- get
                                                  mcpVs <- tlList vs
                                                  let newInfo  = Info (Just vs) (Just mcpVs) (Just j) (Just str) 
                                                      newState = state { labelInfo = newInfo }
                                                  put newState 
                                                  return (toBoolExpr True)

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

-- Translates lists of expressions to MCP collection for the Overton Solver
translateOvertonList :: FDList (FDTerm Int) -> State TLState ModelCol
translateOvertonList (FDList _ vs) = do mcpExprList <- mapM translateTerm vs
                                        return (list mcpExprList)

-- Translates lists of expressions to MCP collection for the Gecode Solver
translateGecodeList :: FDList (FDTerm Int) -> State TLState ModelCol
translateGecodeList l@(FDList i vs) = do state <- get
                                         let varMap = colVarMap state
                                         maybe (newColVar l) return (Map.lookup (getKey i) varMap)

-- Creates a new MCP collection variable for the given list
-- Updates the translation state by inserting its MCP representation
-- into the map and incrementing the varref counter
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

-- Creates additional constraints for collection variables describing their size and elements (only used for translateGecode
newColCs :: ModelCol -> [ModelInt] -> [Model]
newColCs col vs = (size col @= cte (length vs)) : newColCs' col vs 0
  where
   newColCs' _   []     _ = []
   newColCs' col (v:vs) n = ((col @!! n) @= v) : newColCs' col vs (n+1) 

-- Translates relational operators to appropriate MCP operators
translateRelOp Equal = (@=)
translateRelOp Diff  = (@/=)
translateRelOp Less  = (@<)

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

solveWithMCP :: MCPSolver -> MCPModel -> MCPLabelInfo -> MCPSolution
solveWithMCP Overton (SM mcpCs) info = solveWithOverton mcpCs info
solveWithMCP Gecode  (SM mcpCs) info = solveWithGecode  mcpCs info

solveWithOverton :: [Model] -> MCPLabelInfo -> MCPSolution
solveWithOverton mcpCs info = case maybeMCPVars of 
  Nothing      -> error "MCPSolver.solveWithOverton: Found no variables for labeling."
  Just mcpVars -> 
    let vars      = fromJust (labelVars info)
        choiceID  = fromJust (labelID info)
        strtgy    = fromJust (strategy info)
        modelTree = toModelTree mcpCs mcpVars
        solutions = snd $ solve dfs it $
          (modelTree :: OvertonTree) >>= labelWith strtgy
    in S (SolInfo (map (map toCurry) solutions) vars choiceID)
  where maybeMCPVars = mcpLabelVars info
{-
solveWithOverton :: [Model] -> MCPLabelInfo -> MCPSolution
solveWithOverton mcpCs info = 
  case maybeMCPVars of 
    Nothing      -> error "MCPSolver.solveWithOverton: Found no variables for labeling."
    Just mcpVars -> let vars      = fromJust (labelVars info)
                        choiceID  = fromJust (labelID info)
                        strtgy    = fromJust (strategy info)
                        modelTree = toModelTree mcpCs mcpVars
                        solutions = snd $ solve dfs it ((modelTree :: OvertonTree {-Tree (FDInstance OvertonFD) ModelCol-}) >>= labelWith strtgy)
                    in S (SolInfo (map (map toCurry) solutions) vars choiceID)
  where maybeMCPVars = mcpLabelVars info
-}
solveWithGecode :: [Model] -> MCPLabelInfo -> MCPSolution
solveWithGecode mcpCs info = 
  case maybeMCPVars of 
    Nothing      -> error "MCPSolver.solveWithGecode: Found no variables for labeling."
    Just mcpVars -> let vars      = fromJust (labelVars info)
                        choiceID  = fromJust (labelID info)
                        strtgy    = fromJust (strategy info)
                        modelTree = toModelTree mcpCs mcpVars
                        solutions = snd $ solve dfs it ((modelTree :: Tree (FDInstance (GecodeWrappedSolver RuntimeGecodeSolver)) ModelCol) >>= labelWith strtgy)
                    in S (SolInfo (map (map toCurry) solutions) vars choiceID)
  where maybeMCPVars = mcpLabelVars info

{-
solveWithMCP :: MCPSolver -> MCPModel -> MCPLabelInfo -> MCPSolution
solveWithMCP Gecode (SM mcpCs) info = trace ("MCP-Solver-Constraints:\n" ++ show mcpCs ++ "\n") $
  case maybeMCPVars of 
    Nothing      -> error "MCPSolver.solveWithMCP: Found no variables for labeling."
    Just mcpVars -> let modelTree = toModelTree mcpCs mcpVars--do toModelTree mcpCs
                                       --return mcpVars
                    in let gecodeSol = solveWithGecode modelTree strat
                       in S (SolverSltn gecodeSol labelVs choiceID)
  where maybeMCPVars = mcpLabelVars info
        choiceID     = fromJust (labelID info)
        strat        = fromJust (strategy info)
        labelVs      = fromJust (labelVars info)
solveWithMCP Overton (SM mcpCs) info = trace ("MCP-Solver-Constraints:\n" ++ show mcpCs ++ "\n") $
  case maybeMCPVars of 
    Nothing      -> error "MCPSolver.solveWithMCP: Found no variables for labeling."
    Just mcpVars -> let modelTree = toModelTree mcpCs mcpVars--do toModelTree mcpCs
                    in let overtonSol = solveWithOverton modelTree strat
                       in S (SolverSltn overtonSol labelVs choiceID)
  where maybeMCPVars = mcpLabelVars info
        choiceID     = fromJust (labelID info)
        strat        = fromJust (strategy info)
        labelVs      = fromJust (labelVars info)

--solveWithGecode :: FDSolver s => Tree (FDInstance s) ModelCol -> LabelingStrategy -> [[CP.C_Int]]
solveWithGecode modelTree strategy = 
  let sol = snd $ solve dfs it ((modelTree :: Tree (FDInstance (GecodeWrappedSolver RuntimeGecodeSolver)) ModelCol) >>= labelWith strategy)
  in map (map toCurry) sol
    
--solveWithOverton :: FDSolver s => Tree (FDInstance s) ModelCol -> LabelingStrategy -> [[CP.C_Int]]
solveWithOverton modelTree strategy = 
  let sol = snd $ solve dfs it ((modelTree :: Tree (FDInstance OvertonFD) ModelCol) >>= labelWith strategy)
  in map (map toCurry) sol
-}

labelWith strategy col = label $ do
  lst <- getColItems col maxBound
  return $ do
    lsti <- colList col $ length lst
    labelling (matchStrategy strategy) lsti
    assignments lsti
{-
label strategy (ColList exprs) = trace (show exprs) $ 
                                 do labelling (matchStrategy strategy) exprs
                                    assignments exprs
label _        _               = error "MCPSolver.label: Invalid labeling variables" 
-}
matchStrategy :: EnumTerm s t => LabelingStrategy -> [t] -> s [t]
matchStrategy FirstFail = firstFail
matchStrategy MiddleOut = middleOut
matchStrategy EndsOut   = endsOut
matchStrategy _         = inOrder

-- Transform a list of MCP constraints into a monadic MCP model tree
toModelTree :: FDSolver s => [Model] -> ModelCol -> Tree (FDInstance s) ModelCol
toModelTree model mcpLabelVars = mapM_ (\m -> addC (Left m)) model >> return mcpLabelVars


{-
solveWithMCP :: MCPSolver -> MCPModel -> MCPLabelInfo -> MCPSolution
solveWithMCP Gecode (SolverConstraints mcpCs) (LabelInfo info) = trace ("MCP-Solver-Constraints:\n" ++ show mcpCs ++ "\n") $
  let colList  = labelVarsSolver info
      lVars = labelVars info 
      lID = fromJust $ labelID info        
      strategy = getLabelStrategy info 
      modelTree = do toModelTree mcpCs
                     return colList
      results = snd $ solve dfs it ((modelTree :: Tree (FDInstance (GecodeWrappedSolver RuntimeGecodeSolver)) ModelCol) >>= label strategy)
  in Results (Solution (map (map toCurry) results) lVars lID)

solveWithMCP Overton (SolverConstraints mcpCs) (LabelInfo info) = trace ("\nMCPConstraints: " ++ show mcpCs ++ "\n") $
  let colList  = labelVarsSolver info
      lVars = labelVars info 
      lID = fromJust $ labelID info        
      strategy = getLabelStrategy info
      modelTree = do toModelTree mcpCs
                     return colList
      results = snd $ solve dfs it ((modelTree :: Tree (FDInstance OvertonFD) ModelCol) >>= label strategy)
  in Results (Solution (map (map toCurry) results) lVars lID)


getLabelStrategy :: EnumTerm s t => MCPLabelInfo -> [t] -> s [t]
getLabelStrategy info = case (chosenStrategy info) of 
  Nothing -> inOrder
  Just s  -> match s
  where match FirstFail = firstFail
        match MiddleOut = middleOut
        match EndsOut   = endsOut
        match _         = inOrder

label strategy (ColList exprs) = trace (show exprs) $ 
                                 do labelling strategy exprs
                                    assignments exprs
label _        _               = error "MCPSolver.label: Invalid labeling variables" 


-}
