{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Solver.OvertonFD (
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
    initState
    ) where

import Prelude hiding (lookup)
import Control.Monad.State.Lazy
import Control.Monad.Trans
import qualified Data.Map as Map
import Data.Map ((!), Map)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

-- The FD monad
newtype OvertonFD a = FD { unFD :: StateT FDState [] a }
    deriving (Monad, MonadPlus, MonadState FDState)

-- FD variables
newtype FDVar = FDVar { unFDVar :: Int } deriving (Ord, Eq)

type VarSupply = FDVar
data VarInfo = VarInfo
     { delayedConstraints :: OvertonFD (), domain :: IntSet }
type VarMap = Map FDVar VarInfo
data FDState = FDState
     { varSupply :: VarSupply, varMap :: VarMap }

-- Run the FD monad and produce a lazy list of possible solutions.
runFD :: OvertonFD a -> FDState -> [a]
runFD fd state = evalStateT (unFD fd) state

updateSolver :: OvertonFD a -> FDState -> [FDState]
updateSolver fd state = execStateT (unFD fd) state

initState :: FDState
initState = FDState { varSupply = FDVar 0, varMap = Map.empty }

-- Get a new FDVar
newVar :: [Int] -> OvertonFD FDVar
newVar d= do
  s <- get
  let v = varSupply s
  put $ s { varSupply = FDVar (unFDVar v + 1) }
  modify $ \s ->
    let vm = varMap s
        vi = VarInfo {
          delayedConstraints = return (),
          domain = IntSet.fromList d}
    in s { varMap = Map.insert v vi vm }
  return v

newVars :: Int -> [Int] -> OvertonFD [FDVar]
newVars n domain = replicateM n (newVar domain)

-- Lookup the current domain of a variable.
lookup :: FDVar -> OvertonFD IntSet
lookup x = do
  s <- get
  return . domain $ varMap s ! x

-- Update the domain of a variable and fire all delayed constraints
-- associated with that variable.
update :: FDVar -> IntSet -> OvertonFD ()
update x i = do
  s <- get
  let vm = varMap s
      vi = vm ! x
  put $ s { varMap = Map.insert x (vi { domain = i}) vm }
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

addBinaryConstraint :: BinaryConstraint -> BinaryConstraint
addBinaryConstraint f x y = do
  let constraint  = f x y
  constraint
  addConstraint x constraint
  addConstraint y constraint

-- Constrain a variable to a particular value.
hasValue :: FDVar -> Int -> OvertonFD ()
var `hasValue` val = do
  vals <- lookup var
  guard $ val `IntSet.member` vals
  let i = IntSet.singleton val
  when (i /= vals) $ update var i

-- Constrain two variables to have the same value.
same :: FDVar -> FDVar -> OvertonFD ()
same = addBinaryConstraint $ \x y -> do
  xv <- lookup x
  yv <- lookup y
  let i = IntSet.intersection xv yv
  guard $ not $ IntSet.null i
  when (i /= xv) $ update x i
  when (i /= yv) $ update y i

-- Constrain two variables to have different values.
different :: FDVar -> FDVar -> OvertonFD ()
different = addBinaryConstraint $ \x y -> do
  xv <- lookup x
  yv <- lookup y
  guard $ IntSet.size xv > 1 || IntSet.size yv > 1 || xv /= yv
  when (IntSet.size xv == 1 && xv `IntSet.isSubsetOf` yv) $ update y (yv `IntSet.difference` xv)
  when (IntSet.size yv == 1 && yv `IntSet.isSubsetOf` xv) $ update x (xv `IntSet.difference` yv)

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
  xv <- lookup x
  yv <- lookup y
  let xv' = IntSet.filter (< IntSet.findMax yv) xv
      yv' = IntSet.filter (> IntSet.findMin xv) yv
  guard $ not $ IntSet.null xv'
  guard $ not $ IntSet.null yv'
  when (xv /= xv') $ update x xv'
  when (yv /= yv') $ update y yv'

(.<=.) :: FDVar -> FDVar -> OvertonFD ()
(.<=.) = addBinaryConstraint $ \x y -> do
  xv <- lookup x
  yv <- lookup y
  let xv' = IntSet.filter (< (IntSet.findMax yv) + 1) xv
      yv' = IntSet.filter (> (IntSet.findMin xv) - 1) yv
  guard $ not $ IntSet.null xv'
  guard $ not $ IntSet.null yv'
  when (xv /= xv') $ update x xv' 
  when (yv /= yv') $ update y yv'

-- Label variables using a depth-first left-to-right search.
labelling :: [FDVar] -> OvertonFD [Int]
labelling = mapM label where
  label var = do
    vals <- lookup var
    val <- FD . lift $ IntSet.toList vals
    var `hasValue` val
    return val

type DomainUpdate = IntSet -> IntSet -> IntSet

-- add constraints like x `op` y = result
addArithmeticConstraint :: DomainUpdate -> DomainUpdate -> DomainUpdate -> FDVar -> FDVar -> OvertonFD FDVar
addArithmeticConstraint getXDomain getYDomain getZDomain x y = do
  xv <- lookup x
  yv <- lookup y
  z <- newVar (IntSet.toList (getZDomain xv yv))
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
  xv <- lookup x
  yv <- lookup y
  zv <- lookup z
  let zv' = IntSet.intersection zv (getDomain xv yv)
  guard $ not $ IntSet.null zv'
  when (zv /= zv') $ update z zv'

--foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
--foldM1 f (x:xs) = foldM f x xs
--foldM1 _ []     = error "foldM1: empty list"

-- arithmetic constraints
addSum = addArithmeticConstraint getDomainMinus getDomainMinus getDomainPlus

addSub = addArithmeticConstraint getDomainPlus (flip getDomainMinus) getDomainMinus

addMult = addArithmeticConstraint getDomainDiv getDomainDiv getDomainMult

sumList :: [FDVar] -> OvertonFD FDVar
sumList [] = mzero
sumList (x:xs) = foldM addSum x xs  

-- interval arithmetic
getDomainPlus :: IntSet -> IntSet -> IntSet
getDomainPlus xs ys = IntSet.fromList [a..b]
 where
  a = IntSet.findMin xs + IntSet.findMin ys
  b = IntSet.findMax xs + IntSet.findMax ys

getDomainMinus :: IntSet -> IntSet -> IntSet
getDomainMinus xs ys = IntSet.fromList [a..b]
 where
  a = IntSet.findMin xs - IntSet.findMax ys
  b = IntSet.findMax xs - IntSet.findMin ys

getDomainMult :: IntSet -> IntSet -> IntSet
getDomainMult xs ys = IntSet.fromList [a..b]
 where
  a        = minimum products
  b        = maximum products
  products = [x * y | x <- [IntSet.findMin xs, IntSet.findMax xs], y <- [IntSet.findMin ys, IntSet.findMax ys]]

getDomainDiv :: IntSet -> IntSet -> IntSet
getDomainDiv xs ys = IntSet.fromList [a..b]
 where
  a        = minimum (quotients minBound)
  b        = maximum (quotients maxBound)
  quotients z = [if y /= 0 then x `div` y else z |
                  x <- [IntSet.findMin xs, IntSet.findMax xs]
                , y <- [IntSet.findMin ys, IntSet.findMax ys]]
