{-# OPTIONS_GHC -fno-warn-orphans #-}
module Strategies
  ( bfsSearch, dfsSearch, parSearch, fairSearch, conSearch
  , idsSearch, idsDefaultDepth, idsDefaultIncr
  , splitAll, splitLimitDepth, splitAlternating, splitPower
  , bfsParallel
  , dfsBag, fdfsBag, bfsBag, getAllResults, getResult
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
import Control.Concurrent.Bag.Safe
  ( addTask
  , newTaskBag
  , BagT
  , getAllResults
  , getResult
  , TaskBufferSTM
  , Task
  , SplitFunction )
import Control.Concurrent.STM (TChan)
import Control.Concurrent.STM.TStack (TStack)
import Control.Monad.IO.Class
import System.Mem.Weak

import MonadSearch
import MonadList

instance MonadSearch SearchTree where
  constrainMSearch _ _ x = x
  var              x _   = x

dfsBag :: MonadIO m => Maybe (SplitFunction TStack r) -> SearchTree r -> BagT TStack r m a -> m a
dfsBag split = (newTaskBag split) . (:[]) . dfsTask

-- | Fake depth-first search
--   Real depth-first search would use a stack instead of a queue for
--   the task bag.
fdfsBag :: MonadIO m => Maybe (SplitFunction TChan r) -> SearchTree r -> BagT TChan r m a -> m a
fdfsBag split = (newTaskBag split) . (:[]) . dfsTask

dfsTask :: TaskBufferSTM b => SearchTree a -> Task b a (Maybe a)
dfsTask None         = return   Nothing
dfsTask (One v)      = return $ Just v
dfsTask (Choice l r) = do
  addTask $ dfsTask r
  dfsTask l

bfsBag :: MonadIO m => Maybe (SplitFunction TChan r) -> SearchTree r -> BagT TChan r m a -> m a
bfsBag split = (newTaskBag split) . (:[]) . bfs
 where
  bfs None         = return   Nothing
  bfs (One v)      = return $ Just v
  bfs (Choice l r) = do
    addTask $ bfs l
    addTask $ bfs r
    return Nothing

-- | Parallel Breadth-first search
bfsParallel :: SearchTree a -> [a]
bfsParallel t = bfs [t]
 where
  bfs :: [SearchTree a] -> [a]
  bfs [] = []
  bfs xs = runEval $ do
    rs <- evalList rpar $ xs
    return $ values rs ++ (bfs $ children rs)

splitAll :: SearchTree a -> [a]
splitAll None         = []
splitAll (One x )     = [x]
splitAll (Choice l r) = runEval $ do
  rs <- rpar $ splitAll r
  ls <- rseq $ splitAll l
  return $ ls ++ rs

splitLimitDepth :: Int -> SearchTree a -> [a]
splitLimitDepth _ None           = []
splitLimitDepth _ (One x)        = [x]
splitLimitDepth 0 c@(Choice _ _) = dfsSearch c
splitLimitDepth i   (Choice l r) = runEval $ do
  rs <- rpar $ splitLimitDepth (i-1) r
  ls <- rseq $ splitLimitDepth (i-1) l
  return $ ls ++ rs

splitAlternating :: Int -> SearchTree a -> [a]
splitAlternating n = splitAlternating' 1 n
 where
  splitAlternating' :: Int -> Int -> SearchTree a -> [a]
  splitAlternating' _ _ None         = []
  splitAlternating' _ _ (One x)      = [x]
  splitAlternating' 1 n (Choice l r) = runEval $ do
    rs <- rpar $ splitAlternating' n n l
    ls <- rseq $ splitAlternating' n n r
    return $ ls ++ rs
  splitAlternating' i n (Choice l r) =
    let ls = splitAlternating' (i-1) n l
        rs = splitAlternating' (i-1) n r
    in ls ++ rs

splitPower :: SearchTree a -> [a]
splitPower = splitPower' 1 2
 where
  splitPower' _ _ None = []
  splitPower' _ _ (One x) = [x]
  splitPower' 1 n (Choice l r) = runEval $ do
    rs <- rpar $ splitPower' n (n*2) r
    ls <- rseq $ splitPower' n (n*2) l
    return $ ls ++ rs
  splitPower' i n (Choice l r) =
    let ls = splitPower' (i-1) n l
        rs = splitPower' (i-1) n r
    in ls ++ rs

fairSearch :: SearchTree a -> MList IO a
fairSearch = conSearch (-1)

data ExecutionState = Stopped
                 -- number of threads left / running threads
                  | Executing Int [ThreadId]

data ThreadResult a = ThreadStopped ThreadId
                    | Value         a

-- | A parallel search implemented with concurrent Haskell
conSearch :: Int         -- ^ The maximum number of threads to use
         -> SearchTree a -- ^ The search tree to search in
         -> MList IO a
conSearch i tree = do
  threadVar <- newMVar $ Executing i []
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
  removeThread :: ThreadId -> ExecutionState -> ExecutionState
  removeThread tid Stopped = Stopped
  removeThread tid (Executing n tids) =
    Executing (n+1) $ delete tid tids

  killThreads :: MVar ExecutionState -> IO ()
  killThreads threadVar = do
    executeState <- takeMVar threadVar
    case executeState of
      Stopped ->
        hPutStr stderr "Execution already stopped!"
      Executing _ tids ->
        mapM_ (forkIO . killThread) tids
    putMVar threadVar Stopped

  handleResults :: IORef () -> MVar ExecutionState -> Chan (ThreadResult a) -> MList IO a
  handleResults dummyRef threadVar resultChan = do
     -- this is for the optimizer not to optimize out the dummyRef
    writeIORef dummyRef ()
    result <- readChan resultChan
    case result of
      ThreadStopped tid -> do
        state <- modifyMVar threadVar $ return . (\a -> (a,a)) . (removeThread tid)
        case state of
          Executing _ (_:_) ->
            handleResults dummyRef threadVar resultChan
          _ ->
            mnil
      Value a  ->
        mcons a $ handleResults dummyRef threadVar resultChan

  startSearchThread :: MVar ExecutionState -> Chan (ThreadResult a) -> SearchTree a -> IO ()
  startSearchThread threadVar chan tree = do
    executeState <- takeMVar threadVar
    case executeState of
      Stopped -> do
        putMVar threadVar executeState
      Executing 0 tids -> do
        putMVar threadVar executeState
        searchThread threadVar chan tree
      Executing n tids -> do
        newTid <- forkIO $ do
          searchThread threadVar chan tree
          tid <- myThreadId
          writeChan chan $ ThreadStopped tid
        putMVar threadVar $ Executing (n-1) $ newTid:tids

  searchThread :: MVar ExecutionState -> Chan (ThreadResult a) -> SearchTree a -> IO ()
  searchThread threadVar resultChan tree =
    case tree of
      None  ->
        return ()
      One x -> do
        writeChan resultChan $ Value x
      Choice x y -> do
        startSearchThread threadVar resultChan y
        searchThread      threadVar resultChan x

-- |Breadth-first search
bfsSearch :: SearchTree a -> [a]
bfsSearch t = bfs [t]
  where
  bfs [] = []
  bfs ts = values ts ++ bfs (children ts)

values :: [SearchTree a] -> [a]
values []           = []
values (One x : ts) = x : values ts
values (_     : ts) = values ts

children :: [SearchTree a] -> [SearchTree a]
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
