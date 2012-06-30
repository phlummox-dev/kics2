{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module MCPSolver where

import Types
import FDData

import ExternalSolver
import ID

instance ExternalSolver MCPSolver FDConstraint

instance ExternalConstraint FDConstraint
