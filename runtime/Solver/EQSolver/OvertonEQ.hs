{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Solver.EQSolver.OvertonEQ where

import Solver.EQSolver.EQDomain
--import Types

import Control.Monad.State.Lazy
import Control.Monad.Trans
import qualified Data.Map as Map
import Prelude hiding (lookup, EQ)

-- needed for eq solver
-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (ID, getKey, nextNIDs, getConsNr)

-- The OvertonEQ monad
newtype OvertonEQ a = EQ { unEQ :: StateT EQState [] a }
 deriving (Monad, MonadPlus, MonadState EQState)


-- Run the EQ monad and produce a lazy list of possible solutions.
--runFD :: OvertonFD a -> FDState -> [a]
--runEQ :: OvertonEQ [Int] -> EQState -> [[Int]]
runEQ eq state = evalStateT (unEQ eq) state

updateEQ :: OvertonEQ a -> EQState -> [EQState]
updateEQ eq state = execStateT (unEQ eq) state

-- EQ variables
type EQVar = ID

-- EQ state
type EQState = Map.Map Integer VarInfo

initEQState :: EQState
initEQState = Map.empty

-- EQDomain
-- an eq domain consists of:
-- - constrDom: an integer domain for the possible constructors
-- - args: a list of eq variables for possible constructor arguments
data EQDomain = 
  EQDomain
  { eqConstrDom :: Domain
  , eqArgs :: [EQVar]
  }

-- initial EQDomain
initEQDomain :: EQDomain
initEQDomain =
  EQDomain
  { eqConstrDom = toDomain ()
  , eqArgs = []
  }

newEQDomain :: ID -> EQDomain
newEQDomain i = initEQDomain { eqConstrDom = toDomain (0, (getConsNr i) - 1) } 

-- variable information:
-- @delayedConstraints: a variable's constraints
-- @domain: a variable's domain
data VarInfo =
  VarInfo
  { delayedConstraints :: OvertonEQ ()
  , eqDomain :: EQDomain
  }

newInfo :: ID -> VarInfo
newInfo i =
  VarInfo 
  { delayedConstraints = return ()
  , eqDomain = newEQDomain i
  }

emptyInfo :: VarInfo
emptyInfo =
  VarInfo
  { delayedConstraints = return ()
  , eqDomain = initEQDomain
  }

-- Get variable information
getInfo :: EQVar -> OvertonEQ VarInfo
getInfo x = do
  let k = getKey x
  varMap <- get
  case Map.lookup k varMap of
    Nothing   -> do let info = newInfo x
                    put $ Map.insert k info varMap
                    return info
    Just info -> return info

updateInfo :: EQVar -> VarInfo -> OvertonEQ ()
updateInfo x info = do
  let k = getKey x
  varMap <- get
  put $ Map.insert k info varMap

lookupDomArgs :: EQVar -> OvertonEQ (Domain,[EQVar])
lookupDomArgs x = do
  info <- getInfo x
  let dom  = eqConstrDom $ eqDomain info
      args = eqArgs $ eqDomain info
  return (dom,args) 

-- Lookup the current domain of a variable.
lookupDomain :: EQVar -> OvertonEQ Domain
lookupDomain x = fst `liftM` lookupDomArgs x

-- lookup the possible arguments of a variable
lookupArgs :: EQVar -> OvertonEQ [EQVar]
lookupArgs x = snd `liftM` lookupDomArgs x

-- Update the EQDomain of a variable and fire all delayed constraints
-- associated with that variable.
updateEQDomain :: EQVar -> EQDomain -> OvertonEQ ()
updateEQDomain x eqd = do
  info <- getInfo x
  let info' = info { eqDomain = eqd }
  updateInfo x info'
  delayedConstraints info'

--updateDomain x dom = do
--  let k = getKey x
--  varMap <- get
--  case lookup k varMap of
--    Nothing   -> error "OvertonEQ.updateDomain: Unknown variable."
--    Just info -> do put $ Map.insert k (info { domain = dom}) varMap
--                    delayedConstraints info

-- Add a new constraint for a variable to the constraint store.
addConstraint :: EQVar -> OvertonEQ () -> OvertonEQ ()
addConstraint x constraint = do
  info <- getInfo x
  let cs    = delayedConstraints info
      info' = info { delayedConstraints = cs >> constraint }
  updateInfo x info'

--addConstraint x constraint = do
--  let k = getKey x
--  varMap <- get
--  case lookup k varMap of
--    Nothing   ->  error "OvertonEQ.addConstraint: Unknown variable."
--    Just info -> do let cs = delayedConstraints info
--                    put $ Map.insert k (info { delayedConstraints = cs >> constraint }) varMap

type BinaryConstraint = EQVar -> EQVar -> OvertonEQ ()

addBinaryConstraint :: BinaryConstraint -> EQVar -> EQVar -> OvertonEQ ()
addBinaryConstraint f x y = do
  let constraint  = f x y
  constraint
  addConstraint x constraint
  addConstraint y constraint

-- Constrain a variable to a particular value.
hasValue :: EQVar -> (Int,Int) -> OvertonEQ ()
var `hasValue` (c,p) = do
  (dom,args) <- lookupDomArgs var
  guard $ c `member` dom
  guard $ null args || length args == p
  let i     = singleton c
      args' = nextNIDs var p
      eqd   = EQDomain i args'
  when (i /= dom || (null args && p > 0)) $ updateEQDomain var eqd

-- Constrain two variables to have the same value.
same :: EQVar -> EQVar -> OvertonEQ ()
same = addBinaryConstraint $ \x y -> do
  (domX,argsX) <- lookupDomArgs x
  (domY,argsY) <- lookupDomArgs y
  let i  = intersection domX domY
      pX = length argsX
      pY = length argsY          
  guard $ not $ emptyD i
--  guard $ any null [argsX,argsY] || pX == pY
  let (argsX',argsY') = newArgs argsX argsY x y
      eqdX            = EQDomain i argsX'
      eqdY            = EQDomain i argsY'
  when (i /= domX || pY > pX) $ updateEQDomain x eqdX
  when (i /= domY || pX > pY) $ updateEQDomain y eqdY
  zipWithM_ same argsX' argsY'

-- helper function to adapt number of arguments for two variables
newArgs :: [EQVar] -> [EQVar] -> EQVar -> EQVar -> ([EQVar],[EQVar])
newArgs argsX argsY x y
  | pX == pY  = (argsX,argsY)
  | pX >  pY  = (argsX, nextNIDs y pX)
  | otherwise = (nextNIDs x pY, argsY)
  where pX = length argsX
        pY = length argsY

-- Constrain two variables to have different values.
different :: EQVar -> EQVar -> OvertonEQ ()
different = addBinaryConstraint $ \x y -> do
  (domX,argsX) <- lookupDomArgs x
  (domY,argsY) <- lookupDomArgs y
  guard $ not $ isSingleton domX && isSingleton domY && domX == domY
  let (argsX',argsY') = newArgs argsX argsY x y
      eqdX            = EQDomain (domX `difference` domY) argsX'
      eqdY            = EQDomain (domY `difference` domX) argsY'
  when (isSingleton domX && domX `isSubsetOf` domY) $ updateEQDomain y eqdX
  when (isSingleton domY && domY `isSubsetOf` domX) $ updateEQDomain x eqdY
  let cs = zipWith different argsX' argsY'
  foldr1 mplus cs

-- Label variables using a depth-first left-to-right search.
--labelling :: [EQVar] -> OvertonEQ [(Int,[EQVar]]
labelling :: [EQVar] -> OvertonEQ [Int]
labelling = mapM label where
  label var = do
    (vals,args) <- lookupDomArgs var
    val <- EQ . lift $ zip (elems vals) (repeat (length args))
    var `hasValue` val
    return (fst val)

labelOne :: EQVar -> OvertonEQ (Int,Int)
labelOne var = do
  (dom,args) <- lookupDomArgs var
  val <- EQ . lift $ zip (elems dom) (repeat (length args))
  var `hasValue` val
  return val
