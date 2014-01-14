{- 
 - Origin:
 -     Constraint Programming in Haskell 
 -     http://overtond.blogspot.com/2008/07/pre.html
 -     author: David Overton, Melbourne Australia
 -
 - Modifications:
 -     Monadic Constraint Programming
 -     http://www.cs.kuleuven.be/~toms/Haskell/
 -     Tom Schrijvers
 -
 - Further Modifications:
 - Jan Tikovsky
 -}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Solver.Overton.OvertonFD (
    -- Types
    OvertonFD,    -- Monad for finite domain constraint solver
    FDVar,        -- Finite domain solver variable

    -- Functions
    runFD,        -- Run the monad and return a list of solutions.
    newVar,       -- Create a new FDVar
    newVars,      -- Create multiple FDVars
    hasValue,     -- Constrain a FDVar to a specific value
    same,         -- Constrain two FDVars to be the same
    different,    -- Constrain two FDVars to be different
    allDifferent, -- Constrain a list of FDVars to be different
    (.<.),        -- Constrain one FDVar to be less than another
    labelling,    -- Backtracking search for all solutions

-- arithmetic constraints added by Jan Tikovsky
    addSum,
    addSub,
    addMult,
    sumList,
    updateSolver,
    FDState (..),
    initFDState,
    lookupDomain,
    (.<=.)
    ) where

import Debug.Trace

import Prelude hiding (null)
import Control.Monad.State.Lazy
import Control.Monad.Trans
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Map ((!), Map)

import Solver.Overton.OvertonDomain

-- The FD monad
newtype OvertonFD a = FD { unFD :: StateT FDState [] a }
    deriving (Monad, MonadPlus, MonadState FDState)

-- FD variables
newtype FDVar = FDVar { unFDVar :: Int } deriving (Show, Ord, Eq)

type VarSupply = FDVar
data VarInfo = VarInfo
     { delayedConstraints :: OvertonFD (), domain :: Domain }
type VarMap = Map FDVar VarInfo
type CurryVarMap = Map Integer FDVar
data FDState = FDState
     { varSupply :: VarSupply, varMap :: VarMap, curryVarMap :: CurryVarMap }

-- Run the FD monad and produce a lazy list of possible solutions.
runFD :: OvertonFD a -> FDState -> [a]
runFD fd state = evalStateT (unFD fd) state

updateSolver :: OvertonFD a -> FDState -> [FDState]
updateSolver fd state = execStateT (unFD fd) state

initFDState :: FDState
initFDState = FDState { varSupply = FDVar 0, varMap = Map.empty, curryVarMap = Map.empty }

-- Get a new FDVar
newVar :: ToDomain a => a -> OvertonFD FDVar
newVar d = do
  s <- get
  let v = varSupply s
  put $ s { varSupply = FDVar (unFDVar v + 1) }
  modify $ \s ->
    let vm = varMap s
        vi = VarInfo {
          delayedConstraints = return (),
          domain = toDomain d}
    in s { varMap = Map.insert v vi vm }
  return v

newVars :: ToDomain a => Int -> a -> OvertonFD [FDVar]
newVars n domain = replicateM n (newVar domain)

-- Lookup the current domain of a variable.
lookupDomain :: FDVar -> OvertonFD Domain
lookupDomain x = do
  s <- get
  return . domain $ varMap s ! x

-- Update the domain of a variable and fire all delayed constraints
-- associated with that variable.
update :: FDVar -> Domain -> OvertonFD ()
update x d = do
  s <- get
  let vm = varMap s
      vi = vm ! x
  put $ s { varMap = Map.insert x (vi { domain = d}) vm }
  delayedConstraints vi

-- Add a new constraint for a variable to the constraint store.
addConstraint :: FDVar -> OvertonFD () -> OvertonFD ()
addConstraint x constraint = do
  s <- get
  let vm = varMap s
      vi = vm ! x
      cs = delayedConstraints vi
  put $ s { varMap =
    Map.insert x (vi { delayedConstraints = cs >> constraint }) vm }
 
-- Useful helper function for adding binary constraints between FDVars.
type BinaryConstraint = FDVar -> FDVar -> OvertonFD ()

addBinaryConstraint :: BinaryConstraint -> FDVar -> FDVar -> OvertonFD ()
addBinaryConstraint f x y = do
  let constraint  = f x y
  constraint
  addConstraint x constraint
  addConstraint y constraint

-- Constrain a variable to a particular value.
hasValue :: FDVar -> Int -> OvertonFD ()
var `hasValue` val = do
  vals <- lookupDomain var
  guard $ val `member` vals
  let i = singleton val
  when (i /= vals) $ update var i

-- Constrain two variables to have the same value.
same :: FDVar -> FDVar -> OvertonFD ()
same = addBinaryConstraint $ \x y -> do
  xv <- lookupDomain x
  yv <- lookupDomain y
  let i = intersection xv yv
  guard $ not $ null i
  when (i /= xv) $ update x i
  when (i /= yv) $ update y i

-- Constrain two variables to have different values.
different :: FDVar -> FDVar -> OvertonFD ()
different = addBinaryConstraint $ \x y -> do
  xv <- lookupDomain x
  yv <- lookupDomain y
  guard $ not $ isSingleton xv && isSingleton yv && xv == yv
  when (isSingleton xv && xv `isSubsetOf` yv) $ update y (yv `difference` xv)
  when (isSingleton yv && yv `isSubsetOf` xv) $ update x (xv `difference` yv)

-- Constrain a list of variables to all have different values.
allDifferent :: [FDVar] -> OvertonFD ()
allDifferent (x:xs) = do
  mapM_ (different x) xs
  allDifferent xs
allDifferent _ = return ()

-- Constrain one variable to have a value less than the value of another
-- variable.
(.<.) :: FDVar -> FDVar -> OvertonFD ()
(.<.) = addBinaryConstraint $ \x y -> do
  xv <- lookupDomain x
  yv <- lookupDomain y
  let xv' = filterLessThan (findMax yv) xv
      yv' = filterGreaterThan (findMin xv) yv
  guard $ not $ null xv'
  guard $ not $ null yv'
  when (xv /= xv') $ update x xv'
  when (yv /= yv') $ update y yv'

(.<=.) :: FDVar -> FDVar -> OvertonFD ()
(.<=.) = addBinaryConstraint $ \x y -> do
  xv <- lookupDomain x
  yv <- lookupDomain y
  let xv' = filterLessThan ((findMax yv) + 1) xv
      yv' = filterGreaterThan ((findMin xv) - 1) yv
  guard $ not $ null xv'
  guard $ not $ null yv'
  when (xv /= xv') $ update x xv' 
  when (yv /= yv') $ update y yv'
{-
-- Label variables using a depth-first left-to-right search.
labelling :: [FDVar] -> OvertonFD [Int]
labelling = mapM label where
  label var = do
    vals <- lookupDomain var
    val <- FD . lift $ elems vals
    var `hasValue` val
    return val
-}

-- Label variables using a depth-first left-to-right search.
labelling :: [FDVar] -> OvertonFD [Int]
labelling vars = do
  labelingWith vars firstFail
  assignments vars

-- Label variables using the given strategy.
labelingWith :: [FDVar] -> ([FDVar] -> OvertonFD [FDVar]) -> OvertonFD ()
labelingWith [] _ = return ()
labelingWith (v:vs) strategy = do
  labelVar v
  vs' <- strategy vs
  labelingWith vs' strategy
  return ()

-- Label FD variable using a depth-first left-to-right search.
labelVar :: FDVar -> OvertonFD Int
labelVar var = do
  vals <- lookupDomain var
  val <- FD . lift $ elems vals
  var `hasValue` val
  return val

-- in order labeling strategy
inOrder :: [FDVar] -> OvertonFD [FDVar]
inOrder = return

-- first fail labeling strategy
firstFail :: [FDVar] -> OvertonFD [FDVar]
firstFail vars = do
  ds <- mapM (\v -> lookupDomain v >>= (return . size)) vars
  let domVars = sortBy (\x y -> compare (fst x) (fst y)) $ zip ds vars
  return $ map snd domVars

-- Get the assignments for the FD variables
assignments :: [FDVar] -> OvertonFD [Int]
assignments vars = do
  doms <- mapM lookupDomain vars
  guard $ all isSingleton doms
  return (concatMap elems doms)

type DomainUpdate = Domain -> Domain -> Domain

-- add constraints like x `op` y = result
addArithmeticConstraint :: DomainUpdate -> DomainUpdate -> DomainUpdate -> FDVar -> FDVar -> OvertonFD FDVar
addArithmeticConstraint getXDomain getYDomain getZDomain x y = do
  xv <- lookupDomain x
  yv <- lookupDomain y
  z <- newVar (getZDomain xv yv)
  let zConstraint = constraint getZDomain x y z
      xConstraint = constraint getXDomain z y x
      yConstraint = constraint getYDomain z x y
  addConstraint z xConstraint
  addConstraint z yConstraint
  addConstraint x zConstraint
  addConstraint x yConstraint
  addConstraint y zConstraint
  addConstraint y xConstraint
  return z

-- helper function
constraint :: DomainUpdate -> FDVar -> FDVar -> FDVar -> OvertonFD ()
constraint getDomain x y z = do
  xv <- lookupDomain x
  yv <- lookupDomain y
  zv <- lookupDomain z
  let zv' = intersection zv (getDomain xv yv)
  guard $ not $ null zv'
  when (zv /= zv') $ update z zv'

-- arithmetic constraints
addSum = addArithmeticConstraint getDomainMinus getDomainMinus getDomainPlus

addSub = addArithmeticConstraint getDomainPlus (flip getDomainMinus) getDomainMinus

addMult = addArithmeticConstraint getDomainDiv getDomainDiv getDomainMult

sumList :: [FDVar] -> OvertonFD FDVar
sumList xs = do
  z <- newVar 0
  foldM addSum z xs

-- interval arithmetic
getDomainPlus :: Domain -> Domain -> Domain
getDomainPlus xs ys = toDomain (a,b)
 where
  a = findMin xs + findMin ys
  b = findMax xs + findMax ys

getDomainMinus :: Domain -> Domain -> Domain
getDomainMinus xs ys = toDomain (a,b)
 where
  a = findMin xs - findMax ys
  b = findMax xs - findMin ys

getDomainMult :: Domain -> Domain -> Domain
getDomainMult xs ys = toDomain (a,b)
 where
  a        = minimum products
  b        = maximum products
  products = [x * y | x <- [findMin xs, findMax xs], y <- [findMin ys, findMax ys]]

getDomainDiv :: Domain -> Domain -> Domain
getDomainDiv xs ys = toDomain (a,b)
 where
  a        = minimum (quotients minBound)
  b        = maximum (quotients maxBound)
  quotients z = [if y /= 0 then x `div` y else z |
                  x <- [findMin xs, findMax xs]
                , y <- [findMin ys, findMax ys]]
