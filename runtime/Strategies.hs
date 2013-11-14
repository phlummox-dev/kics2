{-# OPTIONS_GHC -fno-warn-orphans #-}
module Strategies
  ( bfsSearch, dfsSearch, parSearch, evalSearch, fairSearch
  , idsSearch, idsDefaultDepth, idsDefaultIncr
  ) where

import Control.Monad.SearchTree
import Control.Parallel.TreeSearch (parSearch)
import Control.Parallel.Strategies
import Data.List (delete)
import Control.Concurrent (forkIO, myThreadId, ThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import MonadSearch

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

data SearchResultMessage a =
    ThreadCreated ThreadId
  | ThreadStopped ThreadId
  | Value a

fairSearch :: SearchTree a -> IO [a]
fairSearch tree = do
  runFlag <- newMVar True
  resultChan <- newChan
  startSearchThread runFlag resultChan tree
  result <- readChan resultChan
  case result of
    ThreadCreated tid ->
      handleResults runFlag resultChan [tid] []
 where
  handleResults :: MVar Bool -> Chan (SearchResultMessage a) -> [ThreadId] -> [ThreadId] -> IO [a]
  handleResults _       _          [] _ =
    return []
  handleResults runFlag resultChan tids stoppedThreads = do
    result <- readChan resultChan
    case result of
      ThreadCreated tid ->
        case elem tid stoppedThreads of
          True ->
            handleResults runFlag resultChan tids (delete tid stoppedThreads)
          False ->
            handleResults runFlag resultChan (tid:tids) stoppedThreads
      ThreadStopped tid ->
        case elem tid tids of
          True ->
            handleResults runFlag resultChan (delete tid tids) stoppedThreads
          False ->
            handleResults runFlag resultChan tids (tid:stoppedThreads)
      Value val         -> do
        other <- handleResults runFlag resultChan tids stoppedThreads
        return $ val:other

  startSearchThread :: MVar Bool -> Chan (SearchResultMessage a) -> SearchTree a -> IO ()
  startSearchThread runFlag resultChan tree = do
    newTid <- forkIO $ do
      tid <- myThreadId
      run <- readMVar runFlag
      case run of
        True ->
          searchThread runFlag resultChan tree
        False ->
          writeChan resultChan $ ThreadStopped tid
    writeChan resultChan $ ThreadCreated newTid

  searchThread :: MVar Bool -> Chan (SearchResultMessage a) -> SearchTree a -> IO ()
  searchThread runFlag resultChan tree =
    case tree of
      None  -> do 
        tid <- myThreadId
        writeChan resultChan $ ThreadStopped tid
      One x -> do
        tid <- myThreadId
        writeChan resultChan $ Value x
        writeChan resultChan $ ThreadStopped tid
      Choice x y -> do
        startSearchThread runFlag resultChan y
        searchThread      runFlag resultChan x

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
