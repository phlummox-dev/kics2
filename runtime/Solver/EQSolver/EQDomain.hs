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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Solver.EQSolver.EQDomain
  ( Domain, ToDomain (..)
  , member, singleton, isSingleton, isSubsetOf, emptyD
  , intersection, difference, filterLessThan, filterGreaterThan
  , findMin, findMax, elems, size
  , mapDomain) where

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

data Domain
  = Range !Int !Int
  | Set IntSet
 deriving Show

class ToDomain a where
  toDomain :: a -> Domain

instance ToDomain Domain where
  toDomain = id

instance ToDomain IntSet where
  toDomain = Set

instance Integral a => ToDomain [a] where
  toDomain = Set . IntSet.fromList . map fromIntegral

instance (Integral a, Integral b) => ToDomain (a,b) where
  toDomain (l,u) = Range (fromIntegral l) (fromIntegral u)

instance ToDomain () where
  toDomain () = Range 0 (1000000000)

instance Integral a => ToDomain a where
  toDomain x = Range (fromIntegral x) (fromIntegral x)

instance Eq Domain where
  (Range l1 u1) == (Range l2 u2) = l1 == l2 && u1 == u2
  xs            == ys            = elems xs == elems ys

instance Ord Domain where
  (Range l1 u1) <= (Range l2 u2) = l1 <= l2 && u1 <= u2
  (Set xs)      <= (Set ys)      = xs <= ys
  (Set xs)      <= (Range l u)   = xs <= IntSet.fromList [l..u]
  (Range l u)   <= (Set ys)      = IntSet.fromList [l..u] <= ys

instance Num Domain where
  (+)         = domainPlusI
  (-)         = domainMinusI
  (*)         = domainMultI
  abs         = domainAbs
  signum      = domainSignum
  fromInteger = toDomain

size :: Domain -> Int
size (Range l u) = u - l + 1
size (Set set)   = IntSet.size set

member :: Int -> Domain -> Bool
member x (Range l u) = l <= x && x <= u
member x (Set xs)    = x `IntSet.member` xs

isSubsetOf :: Domain -> Domain -> Bool
isSubsetOf (Range xl xu) (Range yl yu)    = xl >= yl && xu <= yu
isSubsetOf (Set xs)      (Set ys)         = xs `IntSet.isSubsetOf` ys
isSubsetOf (Set xs)      yd@(Range yl yu) = isSubsetOf (Range xl xu) yd
 where
  xl = IntSet.findMin xs
  xu = IntSet.findMax xs
isSubsetOf (Range xl xu) (Set ys)         = all (`IntSet.member` ys) [xl .. xu]

elems :: Domain -> [Int]
elems (Range l u) = [l .. u]
elems (Set xs)    = IntSet.elems xs

intersection :: Domain -> Domain -> Domain
intersection (Set xs)      (Set ys)      = Set (xs `IntSet.intersection` ys)
intersection (Range xl xh) (Range yl yh) = Range (max xl yl) (min xh yh)
intersection (Set xs)      (Range yl yh) = Set $ IntSet.filter (\x -> x >= yl && x <= yh) xs
intersection x             y             = intersection y x

union :: Domain -> Domain -> Domain
union (Set xs)      (Set ys)        = Set (xs `IntSet.union` ys)
union (Range xl xh) (Range yl yh) 
  | xh + 1 >= yl || yh+1 >= xl      = Range (min xl yl) (max xh yh)
  | otherwise                       = union (Set $ IntSet.fromList [xl..xh])
                                            (Set $ IntSet.fromList [yl..yh]) 
union x@(Set xs)    y@(Range yl yh) =
  if emptyD x then y 
              else let xmin = IntSet.findMin xs
                       xmax = IntSet.findMax xs
                   in if (xmin + 1 >= yl && xmax - 1 <= yh) 
                         then Range (min xmin yl) (max xmax yh)
                         else union (Set xs) (Set $ IntSet.fromList [yl..yh])
union x y = union y x

difference :: Domain -> Domain -> Domain
difference (Set xs) (Set ys) = Set (xs `IntSet.difference` ys)
difference xd@(Range xl xh) (Range yl yh)
  | yl > xh || yh < xl = xd
  | otherwise          = Set $ IntSet.fromList [x | x <- [xl..xh], x < yl || x > yh]
difference x@(Set xs) (Range yl yh) = Set $ IntSet.filter (\x -> x < yl || x > yh) xs
difference x@(Range xl xh) (Set ys)
  | IntSet.findMin ys > xh || IntSet.findMax ys < xl = Range xl xh
  | otherwise = Set $ IntSet.fromList [x | x <- [xl..xh], not (x `IntSet.member` ys)]

emptyD :: Domain -> Bool
emptyD (Set xs)      = IntSet.null xs
emptyD (Range xl xh) = xl > xh

singleton :: Int -> Domain
singleton x = Range x x

isSingleton :: Domain -> Bool
isSingleton (Set xs)      = (IntSet.size xs) == 1
isSingleton (Range xl xh) = xl == xh

filterLessThan :: Int -> Domain -> Domain
filterLessThan n (Set xs)      = Set $ IntSet.filter (< n) xs
filterLessThan n (Range xl xh) = Range xl (min (n-1) xh)

filterGreaterThan :: Int -> Domain -> Domain
filterGreaterThan n (Set xs)      = Set $ IntSet.filter (> n) xs
filterGreaterThan n (Range xl xh) = Range (max (n+1) xl) xh

findMax :: Domain -> Int
findMax (Set xs)      = IntSet.findMax xs
findMax (Range xl xh) = xh

findMin :: Domain -> Int
findMin (Set xs)      = IntSet.findMin xs
findMin (Range xl xh) = xl

mapDomain :: Domain -> (Int -> [Int]) -> Domain
mapDomain d f = Set $ IntSet.fromList $ concatMap f $ elems d

-- type for interval operations on FD domains
type IntervalOp = Domain -> Domain -> Domain
 
-- arithmetic operations on FD domains

-- interval arithmetic
domainPlusI :: IntervalOp
domainPlusI xs ys = toDomain (a,b)
 where
  a = findMin xs + findMin ys
  b = findMax xs + findMax ys

domainMinusI :: IntervalOp
domainMinusI xs ys = toDomain (a,b)
 where
  a = findMin xs - findMax ys
  b = findMax xs - findMin ys

domainMultI :: IntervalOp
domainMultI xs ys = toDomain (a,b)
 where
  a        = minimum products
  b        = maximum products
  products = [x * y | x <- [findMin xs, findMax xs], y <- [findMin ys, findMax ys]]

-- TODO: Solution for domains ranging over negative and positive integer number
--       and ranges containing zero
domainDivI :: Domain -> Domain -> Domain
domainDivI xs ys = toDomain (a,b)
 where
  a        = minimum (quotients minBound)
  b        = maximum (quotients maxBound)
  quotients z = [if y /= 0 then x `div` y else z |
                --[if 0 `member` ys then z else x `div` y |
                  x <- [findMin xs, findMax xs]
                , y <- [findMin ys, findMax ys]]

-- abs on domains
domainAbs :: Domain -> Domain
domainAbs d@(Range l u)  | l >= 0     = d
                         | u <  0     = Range (abs u) (abs l)
                         | otherwise  = Range 0 (max (abs l) u)
domainAbs d@(Set s)      | findMin d >= 0  = d
                         | otherwise       = Set $ IntSet.map abs s

-- signum on domains
domainSignum (Range l u) = Range (signum l) (signum u)
domainSignum set         = Range (signum l) (signum u)
 where
  l = findMin set
  u = findMax set

