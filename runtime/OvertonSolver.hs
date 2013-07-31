{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module OvertonSolver (OvertonSolver) where

import ExternalSolver
import FDData (FDConstraint)
import MCPUtils
import Types

import Control.CP.ComposableTransformers as MCP (solve)
import Control.CP.FD.FD (FDInstance)
import Control.CP.FD.Model (Model, ModelCol)
import Control.CP.FD.OvertonFD.OvertonFD (OvertonFD)
import Control.CP.FD.OvertonFD.Sugar
import Control.CP.FD.Solvers (dfs, it)
import Control.CP.SearchTree (Tree)
import Data.Expr.Sugar (list)

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
    else do let modelTree = toModelTree model (getMCPLabelVars info)
                solutions = snd $ MCP.solve dfs it $ 
                    (modelTree :: OvertonTree) >>= labelWith (getStrategy info)
                cints     = map (map toCurry) solutions
            return $ Solutions cints (getLabelVarIDs info) (getLabelID info)
