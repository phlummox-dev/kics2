{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GecodeSolver (GecodeSolver) where

import ExternalSolver
import FDData (FDConstraint)
import MCPUtils
import Types

import Control.CP.ComposableTransformers as MCP (solve)
import Control.CP.FD.FD (FDInstance)
import Control.CP.FD.Gecode.Common (GecodeWrappedSolver)
import Control.CP.FD.Gecode.Runtime (RuntimeGecodeSolver)
import Control.CP.FD.Model (asCol, cte, Model, ModelInt, ModelCol, ModelColTerm(..), ModelFunctions)
import Control.CP.FD.Solvers (dfs, it)
import Control.CP.SearchTree (Tree)
import Data.Expr.Sugar ((@=), (@!!), size)

import Control.Monad.State
import qualified Data.Map as Map

-- ---------------------------------------------------------------------------
-- Solver Monads and ExternalSolver instances
-- ---------------------------------------------------------------------------

-- the gecode solver monad
newtype GecodeSolver a = Gecode { gecodeSolver :: State MCPState a }
 deriving (Monad, MonadState MCPState)

instance ExternalSolver GecodeSolver where
  type ForConstraint GecodeSolver = FDConstraint
  type SolverModel   GecodeSolver = [Model]
  type Solutions     GecodeSolver = MCPSolutions

  translate    = mapM (translateMCP translateGecodeList)
  solve        = solveWithGecode
  makeBindings = makeBindingsMCP
  run          = flip evalState initial . gecodeSolver

-- ---------------------------------------------------------------------------
-- Translation to MCP model
-- ---------------------------------------------------------------------------

-- Translates a list of fd terms to a MCP collection for the Gecode Solver
translateGecodeList :: FDList (Term Int) -> GecodeSolver ModelCol
translateGecodeList list@(FDList i _) = do 
  state <- get
  maybe (newColVar list) return (Map.lookup (getKey i) (colVarMap state))

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

-- ---------------------------------------------------------------------------
-- Solving MCP model
-- ---------------------------------------------------------------------------

type GecodeTree 
  = Tree (FDInstance (GecodeWrappedSolver RuntimeGecodeSolver)) ModelCol

solveWithGecode :: [Model] -> GecodeSolver MCPSolutions
solveWithGecode model = do
  state <- get
  let info = labelInfo state
  if (isNotLabelled info) 
    then error "MCPSolver.solveWithGecode: Found no variables for labeling."
    else do let addCs     = additionalCs state
                modelTree = toModelTree (model ++ addCs) (getMCPLabelVars info)
                solutions = snd $ MCP.solve dfs it $ 
                    (modelTree :: GecodeTree) >>= labelWith (getStrategy info)
                cints     = map (map toCurry) solutions
            return $ Solutions cints (getLabelVarIDs info) (getLabelID info)
