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
import Control.CP.FD.Model (Model, ModelCol)
import Control.CP.FD.Solvers (dfs, it)
import Control.CP.SearchTree (Tree)

import Control.Monad.State

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

  translate    = mapM translateMCP
  solve        = solveWithGecode
  makeBindings = makeBindingsMCP
  run          = flip evalState initial . gecodeSolver

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
    else do let modelTree = toModelTree model (getMCPLabelVars info)
                solutions = snd $ MCP.solve dfs it $ 
                    (modelTree :: GecodeTree) >>= labelWith (getStrategy info)
                cints     = map (map toCurry) solutions
            return $ Solutions cints (getLabelVarIDs info) (getLabelID info)
