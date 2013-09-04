{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Solver.OvertonSolver (OvertonSolver) where

import Solver.ExternalSolver
import Solver.Constraints (FDConstraint)
import Solver.MCPUtils
import Types

import Control.CP.ComposableTransformers as MCP (solve)
import Control.CP.FD.FD (FDInstance)
import Control.CP.FD.Model (Model, ModelCol)
import Control.CP.FD.OvertonFD.OvertonFD (OvertonFD)
import Control.CP.FD.OvertonFD.Sugar
import Control.CP.FD.Solvers (dfs, it)
import Control.CP.SearchTree (Tree (Return))
import Data.Expr.Data (ColExpr (ColList))

import Control.Monad.State

-- ---------------------------------------------------------------------------
-- Solver Monad and ExternalSolver instance
-- ---------------------------------------------------------------------------

-- the overton solver monad
newtype OvertonSolver a = Overton { overtonSolver :: State MCPState a }
 deriving (Monad, MonadState MCPState)

instance ExternalSolver OvertonSolver where
  type ForConstraint OvertonSolver = FDConstraint
  type SolverModel   OvertonSolver = [Model]
  type Solutions     OvertonSolver = MCPSolutions

  translate    = mapM translateMCP
  solve        = solveWithOverton
  makeBindings = makeBindingsMCP
  run          = flip evalState initial . overtonSolver

-- ---------------------------------------------------------------------------
-- Solving MCP model
-- ---------------------------------------------------------------------------

type OvertonTree = Tree (FDInstance OvertonFD) ModelCol

solveWithOverton :: [Model] -> OvertonSolver MCPSolutions
solveWithOverton model = do
  state <- get
  let info = labelInfo state
  if (isNotLabelled info) 
    then error "MCPSolver.solveWithOverton: Found no variables for labeling."
    else do case (toModelTree (getMCPLabelVars info) model) of 
              (Return (ColList col)) -> mkSolution [col]
              modelTree              -> mkSolution $ snd $ MCP.solve dfs it $               
                    (modelTree :: OvertonTree) >>= labelWith (getStrategy info)
