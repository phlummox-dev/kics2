{-# OPTIONS_GHC -fno-warn-orphans #-}
module Strategies
  ( bfsSearch, dfsSearch, parSearch, evalSearch, fairSearch
  , idsSearch, idsDefaultDepth, idsDefaultIncr
  ) where

import System.IO (hPutStr, stderr)
import Control.Monad.SearchTree
import Control.Parallel.TreeSearch (parSearch)
import Control.Parallel.Strategies
import Data.IORef (newIORef, mkWeakIORef, IORef, writeIORef)
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

data ExecuteState = Stopped
                  | Executing [ThreadId]

fairSearch :: SearchTree a -> MList IO a
fairSearch tree = do
  threadVar <- newMVar $ Executing []
  resultChan <- newChan

  -- The following dummy IORef is just used to have a trigger for the
  -- finalizer to kill remaining threads.
  -- The naive implementation, using a weak reference on the Chan does not work
  -- as expected. The problem is the following:
  --   The threads need to be killed when there is no reference on the IOList
  --   anymore. This will be done in the finalizer "killThreads threadVar".
  --   Setting this as a finalizer for the result Chan requires using weak
  --   references to the Chan in the search threads. The GHC runtime now detects
  --   that there is only one reference to the Chan, the reference in
  --   handleResults. Here the program wants to read from the Chan and the GHC
  --   runtime detects that this will be never successful (as there is no
  --   reference to the Chan in another thread). The GHC runtime stops the
  --   waiting thread, the last reference to the Chan is removed, the garbage
  --   collector fires the finalizer and all search threads are killed.
  dummyRef <- newIORef ()
  _ <- mkWeakIORef dummyRef $ killThreads threadVar

  startSearchThread threadVar resultChan tree
  handleResults dummyRef threadVar resultChan
 where
  removeThread :: ThreadId -> ExecuteState -> ExecuteState
  removeThread tid Stopped = Stopped
  removeThread tid (Executing tids) =
    Executing $ delete tid tids

  killThreads :: MVar ExecuteState -> IO ()
  killThreads threadVar = do
    executeState <- takeMVar threadVar
    case executeState of
      Stopped ->
        hPutStr stderr "Execution already stopped!"
      Executing tids ->
        mapM_ killThread tids
    putMVar threadVar Stopped

  handleResults :: IORef () -> MVar ExecuteState -> Chan (Maybe a) -> MList IO a
  handleResults dummyRef threadVar resultChan = do
    executeState <- readMVar threadVar
     -- this is for the optimizer not to optimize out the dummyRef
    writeIORef dummyRef ()
    case executeState of
      Executing (_:_) -> do
        result <- readChan resultChan
        case result of
          Nothing -> handleResults dummyRef threadVar resultChan
          Just a  -> mcons a $ handleResults dummyRef threadVar resultChan
      _ ->
        mnil

  startSearchThread :: MVar ExecuteState -> (Chan (Maybe a)) -> SearchTree a -> IO ()
  startSearchThread threadVar chan tree = do
    executeState <- takeMVar threadVar
    case executeState of
      Stopped -> do
        putMVar threadVar executeState
      Executing tids -> do
        newTid <- forkIO $ do
          tid <- myThreadId
          searchThread threadVar chan tree
          modifyMVar_ threadVar $ return . (removeThread tid)
        putMVar threadVar $ Executing $ newTid:tids

  searchThread :: MVar ExecuteState -> (Chan (Maybe a)) -> SearchTree a -> IO ()
  searchThread threadVar resultChan tree =
    case tree of
      None  -> writeChan resultChan $ Nothing
      One x -> writeChan resultChan $ Just x
      Choice x y -> do
        startSearchThread threadVar resultChan y
        searchThread      threadVar resultChan x

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
