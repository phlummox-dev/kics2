{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Solver.GecodeSolver (GecodeSearchSolver, GecodeRuntimeSolver) where

import Solver.Constraints (FDConstraint)
import Solver.ExternalSolver
import Solver.MCPUtils
import Types

import Control.CP.ComposableTransformers as MCP (solve)
import Control.CP.FD.FD (FDInstance)
import Control.CP.FD.GecodeExample (setSearchMinimize)
import Control.CP.FD.Gecode.Common (GecodeWrappedSolver)
import Control.CP.FD.Gecode.Runtime (RuntimeGecodeSolver)
import Control.CP.FD.Gecode.RuntimeSearch (SearchGecodeSolver)
import Control.CP.FD.Interface (labelCol)
import Control.CP.FD.Model (Model, ModelCol)
import Control.CP.FD.Solvers (dfs, it)
import Control.CP.SearchTree (Tree (Return))
import Data.Expr.Data (ColExpr (ColList))

import Control.Monad.State

-- ---------------------------------------------------------------------------
-- Solver Monads and ExternalSolver instances
-- ---------------------------------------------------------------------------

-- the gecode solver monad for the search solver
newtype GecodeSearchSolver a = GecodeSearch { gecodeSearchSolver :: State MCPState a }
 deriving (Monad, MonadState MCPState)

instance ExternalSolver GecodeSearchSolver where
  type ForConstraint GecodeSearchSolver = FDConstraint
  type SolverModel   GecodeSearchSolver = [Model]
  type Solutions     GecodeSearchSolver = MCPSolutions

  translate    = mapM translateMCP
  solve        = solveWithGecodeSearch
  makeBindings = makeBindingsMCP
  run          = flip evalState initial . gecodeSearchSolver


-- the gecode solver monad for the runtime solver
newtype GecodeRuntimeSolver a = GecodeRuntime { gecodeRuntimeSolver :: State MCPState a }
 deriving (Monad, MonadState MCPState)

instance ExternalSolver GecodeRuntimeSolver where
  type ForConstraint GecodeRuntimeSolver = FDConstraint
  type SolverModel   GecodeRuntimeSolver = [Model]
  type Solutions     GecodeRuntimeSolver = MCPSolutions

  translate    = mapM translateMCP
  solve        = solveWithGecodeRuntime
  makeBindings = makeBindingsMCP
  run          = flip evalState initial . gecodeRuntimeSolver

-- ---------------------------------------------------------------------------
-- Solving MCP model
-- ---------------------------------------------------------------------------

type GecodeRuntimeTree 
  = Tree (FDInstance (GecodeWrappedSolver RuntimeGecodeSolver)) ModelCol

type GecodeSearchTree
  = Tree (FDInstance (GecodeWrappedSolver SearchGecodeSolver)) ModelCol

solveWithGecodeRuntime :: [Model] -> GecodeRuntimeSolver MCPSolutions
solveWithGecodeRuntime model = do
  state <- get
  let info = labelInfo state
  if (isNotLabelled info) 
    then error "MCPSolver.solveWithGecode: Found no variables for labeling."
    else do case (toModelTree (getMCPLabelVars info) model) of 
              (Return (ColList col)) -> mkSolution [col]
              modelTree              -> mkSolution $ snd $ MCP.solve dfs it $               
                    (modelTree :: GecodeRuntimeTree) >>=  labelWith (getStrategy info)

solveWithGecodeSearch :: [Model] -> GecodeSearchSolver MCPSolutions
solveWithGecodeSearch model = do
  state <- get
  let info = labelInfo state
  if (isNotLabelled info) 
    then error "MCPSolver.solveWithGecode: Found no variables for labeling."
    else do case (toModelTree (getMCPLabelVars info) model) of 
              (Return (ColList col)) -> mkSolution [col]
              modelTree              -> mkSolution $ snd $ MCP.solve dfs it $               
                    (modelTree :: GecodeSearchTree) >>= (\x -> setSearchMinimize >> return x) >>= labelCol
