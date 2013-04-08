{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- necessary to prevent import cycles

module MCPSolver where

import Types
import FDData

import ExternalSolver
import ID

instance ExternalFDSolver MCPSolver FDConstraint

instance WrappableConstraint FDConstraint
