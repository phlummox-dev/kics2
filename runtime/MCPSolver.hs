{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MCPSolver where

import Types
import FDData
import qualified Curry_Prelude as CP

import Data.Expr.Data hiding (Const,Plus,Minus,Mult)
import Data.Expr.Sugar
import Control.CP.ComposableTransformers (solve)
import Control.CP.SearchTree (addC, Tree (..))
import Control.CP.EnumTerm
import Control.CP.FD.Model
import Control.CP.FD.FD (FDInstance, FDSolver)
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
  updateVars = updateFDConstr getVarBinding

-- ---------------------------------------------------------------------------
-- ExternalSolver instance for MCP Solvers
-- ---------------------------------------------------------------------------


instance ExternalSolver MCPSolver FDConstraint where
  newtype SolverModel MCPSolver FDConstraint = SM [Model]

  -- |Type for storing labeling information for the MCP solvers:
  -- @labelVars    - labeling variables in original representation
  -- @mcpLabelVars - labeling variables translated into corresponding MCP representation
  -- @choiceID     - fresh ID, necessary for constructing choices over solutions, when transforming
  --                 solver solutions into binding constraints
  -- @strategy     - labeling strategy
  data LabelInfo MCPSolver FDConstraint = Info { labelVars    :: Maybe [Term Int]
                                               , mcpLabelVars :: Maybe ModelCol
                                               , choiceID     :: Maybe ID
                                               , strategy     :: Maybe LabelingStrategy
                                               }
  newtype Solutions MCPSolver FDConstraint = S (SolverSolution CP.C_Int (Term Int))

  translate _ fdCs = translateToMCP fdCs

  solveWith = solveWithMCP 

  makeConstrSolutions _ (S solutions) e = bindSolution solutions e

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

-- Translation state for Haskell's state monad
-- @intVarMap     - Table of already translated constraint variables
-- @nextIntVarRef - Next variable reference
-- @labelInfo     - collected labeling information
data TLState = TLState { 
  intVarMap     :: IntVarMap,
  nextIntVarRef :: Int,
  labelInfo     :: MCPLabelInfo
}

-- Initial state
baseTLState :: TLState
baseTLState = TLState { 
  intVarMap     = Map.empty,
  nextIntVarRef = 0,
  labelInfo     = baseLabelInfo
}

-- Initial (empty) labeling information
baseLabelInfo :: MCPLabelInfo
baseLabelInfo = Info {
  labelVars    = Nothing,
  mcpLabelVars = Nothing,
  choiceID     = Nothing,
  strategy     = Nothing
}

-- Translates list of finite domain constraints into a MCP model
-- and collects labeling information if available
-- using Haskell's state monad
translateToMCP :: [FDConstraint] -> (MCPModel,MCPLabelInfo)
translateToMCP fdCs = trace ("\nFDConstraints:\n" ++ show fdCs ++ "\n") $
                      let (mcpCs,state) = runState (mapM translateConstr fdCs) baseTLState
                          info = labelInfo state
                      in (SM mcpCs, info)

-- Translates a single finite domain constraint into a specific MCP constraint
-- using Haskell's state monad
translateConstr :: FDConstraint -> State TLState Model
translateConstr (FDRel relop t1 t2)        = do mcpTerm1 <- translateTerm t1
                                                mcpTerm2 <- translateTerm t2
                                                let mcpRelop = translateRelOp relop
                                                return $ mcpRelop mcpTerm1 mcpTerm2
translateConstr (FDArith arithOp t1 t2 r)  = do mcpTerm1  <- translateTerm t1
                                                mcpTerm2  <- translateTerm t2
                                                mcpResult <- translateTerm r
                                                let mcpArithOp = translateArithOp arithOp
                                                return $ (mcpArithOp mcpTerm1 mcpTerm2) @= mcpResult
translateConstr (FDSum vs r)               = do mcpVs     <- translateList vs
                                                mcpResult <- translateTerm r
                                                return $ (xsum mcpVs) @= mcpResult
translateConstr (FDAllDifferent vs)        = do mcpVs <- translateList vs
                                                return $ allDiff mcpVs
translateConstr (FDDomain vs l u)          = do mcpVs <- translateList vs
                                                mcpL  <- translateTerm l
                                                mcpU  <- translateTerm u
                                                let domain varList lower upper = forall varList (\var -> var @: (lower,upper))
                                                return $ domain mcpVs mcpL mcpU
translateConstr (FDLabeling strategy vs i) = do state <- get
                                                mcpVs <- translateList vs
                                                let newInfo  = Info (Just vs) (Just mcpVs) (Just i) (Just strategy) 
                                                    newState = state { labelInfo = newInfo }
                                                put newState 
                                                return (BoolConst True)

-- Translates integer terms to appropriate MCP terms
-- using Haskell's state monad
translateTerm :: Term Int -> State TLState ModelInt
translateTerm (Const x) = return (cte x)
translateTerm v@(Var i) = do state <- get
                             let varMap = intVarMap state
                             maybe (newVar v) return (Map.lookup (getKey i) varMap)

-- Creates a new MCP variable for the given constraint variable
-- Updates the translation state by inserting the MCP representation
-- of the variable into the map and incrementing the varref counter
newVar :: Term Int -> State TLState ModelInt
newVar (Var i) = do state <- get
                    let varMap   = intVarMap state
                        varRef   = nextIntVarRef state
                        nvar     = Term (ModelIntVar varRef)
                        newState = state { nextIntVarRef = varRef + 1
                                         , intVarMap = Map.insert (getKey i) nvar varMap
                                         }
                    put newState
                    return nvar

-- Translates lists of expressions to MCP collection
translateList :: [Term Int] -> State TLState ModelCol
translateList vs = do mcpExprList <- mapM translateTerm vs
                      return (ColList mcpExprList)

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

solveWithMCP :: MCPSolver -> MCPModel -> MCPLabelInfo -> MCPSolution
solveWithMCP Gecode (SM mcpCs) info = trace ("MCP-Solver-Constraints:\n" ++ show mcpCs ++ "\n") $
  case maybeMCPVars of 
    Nothing      -> error "MCPSolver.solveWithMCP: Found no variables for labeling."
    Just mcpVars -> let modelTree = toModelTree mcpCs mcpVars--do toModelTree mcpCs
                                       --return mcpVars
                    in let gecodeSol = solveWithGecode modelTree strat
                       in S (Sol gecodeSol labelVs labelID)
  where maybeMCPVars = mcpLabelVars info
        strat        = fromJust (strategy info)
        labelVs      = fromJust (labelVars info)
        labelID      = fromJust (choiceID info)
solveWithMCP Overton (SM mcpCs) info = trace ("MCP-Solver-Constraints:\n" ++ show mcpCs ++ "\n") $
  case maybeMCPVars of 
    Nothing      -> error "MCPSolver.solveWithMCP: Found no variables for labeling."
    Just mcpVars -> let modelTree = toModelTree mcpCs mcpVars--do toModelTree mcpCs
                    in let overtonSol = solveWithOverton modelTree strat
                       in S (Sol overtonSol labelVs labelID)
  where maybeMCPVars = mcpLabelVars info
        strat        = fromJust (strategy info)
        labelVs      = fromJust (labelVars info)
        labelID      = fromJust (choiceID info)

--solveWithGecode :: FDSolver s => Tree (FDInstance s) ModelCol -> LabelingStrategy -> [[CP.C_Int]]
solveWithGecode modelTree strategy = 
  let sol = snd $ solve dfs it ((modelTree :: Tree (FDInstance (GecodeWrappedSolver RuntimeGecodeSolver)) ModelCol) >>= label strategy)
  in map (map toCurry) sol
    
--solveWithOverton :: FDSolver s => Tree (FDInstance s) ModelCol -> LabelingStrategy -> [[CP.C_Int]]
solveWithOverton modelTree strategy = 
  let sol = snd $ solve dfs it ((modelTree :: Tree (FDInstance OvertonFD) ModelCol) >>= label strategy)
  in map (map toCurry) sol

label strategy (ColList exprs) = trace (show exprs) $ 
                                 do labelling (matchStrategy strategy) exprs
                                    assignments exprs
label _        _               = error "MCPSolver.label: Invalid labeling variables" 

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
