{-# OPTIONS_GHC -fno-warn-orphans #-}
module Strategies
  ( bfsSearch, dfsSearch, parSearch, evalSearch, fairSearch
  , idsSearch, idsDefaultDepth, idsDefaultIncr
  ) where

import Control.Monad.SearchTree
import Control.Parallel.TreeSearch (parSearch)
import Control.Parallel.Strategies
import Data.List (delete)
import Control.Concurrent (forkIO, myThreadId, ThreadId, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import System.Mem.Weak

import MonadSearch
import MonadList

instance MonadSearch SearchTree where
  constrainMSearch _ _ x = x
  var              x _   = x

evalSearch :: SearchTree a -> [a]
evalSearch None         = []
evalSearch (One x )     = [x]
evalSearch (Choice l r) = runEval $ do
  rs <- rpar $ evalSearch r
  ls <- rseq $ evalSearch l
  return $ ls ++ rs

fairSearch :: SearchTree a -> MList IO a
fairSearch tree = do
  threadVar <- newMVar $ ([], [])
  resultChan <- newChan
  weakChan <- mkWeakPtr resultChan $ Just $ killThreads threadVar
  startSearchThread threadVar weakChan tree
  handleResults threadVar resultChan
 where
  addThread :: ThreadId -> ([ThreadId], [ThreadId]) -> ([ThreadId], [ThreadId])
  addThread tid (runningThreads, stoppedThreads) =
    case elem tid stoppedThreads of
      True -> (runningThreads, delete tid stoppedThreads)
      False -> (tid:runningThreads, stoppedThreads)

  removeThread :: ThreadId -> ([ThreadId], [ThreadId]) -> ([ThreadId], [ThreadId])
  removeThread tid (runningThreads, stoppedThreads) =
    case elem tid runningThreads of
      True -> (delete tid runningThreads, stoppedThreads)
      False -> (runningThreads, tid:stoppedThreads)

  killThreads :: MVar ([ThreadId], [ThreadId]) -> IO ()
  killThreads threadVar = do
    (runningThreads, _) <- takeMVar threadVar
    mapM_ killThread runningThreads

  handleResults :: MVar ([ThreadId], [ThreadId]) -> Chan a -> MList IO a
  handleResults threadVar resultChan = do
    threads@(runningThreads, _) <- readMVar threadVar
    case runningThreads of
      [] -> mnil
      _  -> do
        result <- readChan resultChan
        mcons result $ handleResults threadVar resultChan

  startSearchThread :: MVar ([ThreadId], [ThreadId]) -> Weak (Chan a) -> SearchTree a -> IO ()
  startSearchThread threadVar weakChan tree = do
    threads <- takeMVar threadVar
    newTid <- forkIO $ do
      searchThread threadVar weakChan tree
      tid <- myThreadId
      modifyMVar_ threadVar $ return . (removeThread tid)
    putMVar threadVar (addThread newTid threads)

  searchThread :: MVar ([ThreadId], [ThreadId]) -> Weak (Chan a) -> SearchTree a -> IO ()
  searchThread threadVar weakChan tree =
    case tree of
      None  -> 
        return ()
      One x -> do
        mResultChan <- deRefWeak weakChan
        case mResultChan of
          Nothing -> return ()
          Just resultChan ->
            writeChan resultChan x
      Choice x y -> do
        startSearchThread threadVar weakChan y
        searchThread      threadVar weakChan x

-- |Breadth-first search
bfsSearch :: SearchTree a -> [a]
bfsSearch t = bfs [t]
  where
  bfs [] = []
  bfs ts = values ts ++ bfs (children ts)

  values []           = []
  values (One x : ts) = x : values ts
  values (_     : ts) = values ts

  children []                = []
  children (Choice x y : ts) = x : y : children ts
  children (_          : ts) = children ts

-- |Depth first search
dfsSearch :: SearchTree a -> [a]
dfsSearch None         = []
dfsSearch (One      x) = [x]
dfsSearch (Choice x y) = dfsSearch x ++ dfsSearch y

idsDefaultDepth :: Int
idsDefaultDepth = 100

idsDefaultIncr :: Int -> Int
idsDefaultIncr = (*2)

-- |Return all values in a search tree via iterative-deepening search.
-- The first argument is the initial depth bound and
-- the second argument is a function to increase the depth in each
-- iteration.
idsSearch :: Int -> (Int -> Int) -> SearchTree a -> [a]
idsSearch initdepth incr st = iterIDS initdepth (collectBounded 0 initdepth st)
  where
  iterIDS _ Nil         = []
  iterIDS n (Cons x xs) = x : iterIDS n xs
  iterIDS n Abort       = let newdepth = incr n
                          in  iterIDS newdepth (collectBounded n newdepth st)

-- |Collect solutions within some level bounds in a tree.
collectBounded :: Int -> Int -> SearchTree a -> AbortList a
collectBounded oldbound newbound st = collectLevel newbound st
  where
  collectLevel _ None          = Nil
  collectLevel d (One      x)
    | d <= newbound - oldbound = x `Cons` Nil
    | otherwise                = Nil
  collectLevel d (Choice x y)
    | d > 0     = collectLevel (d-1) x +! collectLevel (d-1) y
    | otherwise = Abort

-- |List containing 'Abort's to implement the iterative depeening strategy
data AbortList a = Nil | Cons a (AbortList a) | Abort

-- |Concatenation on abort lists where aborts are moved to the right
(+!) :: AbortList a -> AbortList a -> AbortList a
(+!) Abort       Abort       = Abort
(+!) Abort       Nil         = Abort
(+!) Abort       (Cons x xs) = x `Cons` (Abort +! xs)
(+!) Nil         ys          = ys
(+!) (Cons x xs) ys          = x `Cons` (xs +! ys)
