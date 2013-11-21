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

data SearchResultMessage a =
    ThreadCreated ThreadId
  | ThreadStopped ThreadId
  | Value a
  
type ThreadsRef a = MVar ([ThreadId], [ThreadId])

fairSearch :: SearchTree a -> MList IO a
fairSearch tree = do
  runFlag <- newMVar True
  resultChan <- newChan
  startSearchThread runFlag resultChan tree
  result <- readChan resultChan
  case result of
    ThreadCreated tid -> do
      thvar <- newMVar ([tid], [])
      _ <- mkWeakMVar thvar (killThreads runFlag resultChan thvar)
      handleResults runFlag resultChan thvar
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

  killThreads :: MVar Bool -> Chan (SearchResultMessage a) -> ThreadsRef a -> IO ()
  killThreads runFlag resultChan thvar = do
    _ <- swapMVar runFlag False
    _ <- handleThreadChanges resultChan thvar
    (runningThreads, _) <- takeMVar thvar
    mapM_ killThread runningThreads

  handleThreadChanges :: Chan (SearchResultMessage a) -> ThreadsRef a -> IO ()
  handleThreadChanges resultChan thvar = do
    result <- readChan resultChan
    case result of
      ThreadCreated tid -> do
        modifyMVar_ thvar $ return . (addThread tid)
        handleThreadChanges resultChan thvar
      ThreadStopped tid -> do
        modifyMVar_ thvar $ return . (removeThread tid)
        handleThreadChanges resultChan thvar
      _ ->
        handleThreadChanges resultChan thvar

  handleResults :: MVar Bool -> Chan (SearchResultMessage a) -> ThreadsRef a -> MList IO a
  handleResults runFlag resultChan thvar = do
    threads@(runningThreads, _) <- readMVar thvar
    case runningThreads of
      [] -> mnil
      _  -> do
        result <- readChan resultChan
        case result of
          ThreadCreated tid -> do
            modifyMVar_ thvar $ return . (addThread tid)
            handleResults runFlag resultChan thvar
          ThreadStopped tid -> do
            modifyMVar_ thvar $ return . (removeThread tid)
            handleResults runFlag resultChan thvar
          Value val ->
            mcons val $ handleResults runFlag resultChan thvar

  startSearchThread :: MVar Bool -> Chan (SearchResultMessage a) -> SearchTree a -> IO ()
  startSearchThread runFlag resultChan tree = do
    run <- takeMVar runFlag
    newTid <- forkIO $ do
      tid <- myThreadId
      run <- readMVar runFlag
      case run of
        True ->
          searchThread runFlag resultChan tree
        False ->
          writeChan resultChan $ ThreadStopped tid
    writeChan resultChan $ ThreadCreated newTid
    putMVar runFlag run

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
