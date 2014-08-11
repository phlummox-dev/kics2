{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Strategies
  ( bfsSearch, dfsSearch, fairSearch, conSearch
  , idsSearch, idsDefaultDepth, idsDefaultIncr
  , splitAll, bfsParallel
  , dfsBag, fdfsBag, bfsBag, fairBag, getAllResults, getResult
  , dfsBagLazy, fdfsBagLazy, bfsBagLazy, fairBagLazy
  ) where

import System.IO (hPutStr, stderr)
import Control.Monad.SearchTree
import Control.Parallel.Strategies
import Data.IORef (newIORef, mkWeakIORef, IORef, writeIORef)
import Data.List (delete, partition)
import Data.Typeable (Typeable)
import Control.Monad (liftM, void)
import Control.Exception (Exception, uninterruptibleMask_, throwTo, catch, catches, try, evaluate, Handler (..))
import Control.Concurrent (forkIO, myThreadId, ThreadId, killThread, forkIOWithUnmask)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import qualified Control.Concurrent.Bag.STM as STMBag
import           Control.Concurrent.Bag.STM ( SplitFunction
                                            , BufferType (..)
                                            , Interruptible (..)
                                            , TaskIO
                                            , addTask
                                            , BagT
                                            , getAllResults
                                            , getResult )
import Control.Monad.IO.Class
import System.Mem.Weak
import System.IO.Unsafe (unsafeInterleaveIO)

import MonadSearch
import MonadList

instance MonadSearch SearchTree where
  constrainMSearch _ _ x = x
  var              x _   = x

dfsBag :: MonadIO m => SearchTree r -> BagT r m a -> m a
dfsBag = (STMBag.runTaskBag Stack $ Just STMBag.takeFirst) . (:[]) . dfsTask

dfsBagLazy :: SearchTree r -> IO [r]
dfsBagLazy = (STMBag.lazyTaskBag Stack $ Just STMBag.takeFirst) . (:[]) . dfsTask

-- | Fake depth-first search
--   Real depth-first search would use a stack instead of a queue for
--   the task bag.
fdfsBag :: MonadIO m => SearchTree r -> BagT r m a -> m a
fdfsBag = (STMBag.runTaskBag Queue $ Just STMBag.takeFirst) . (:[]) . dfsTask

fdfsBagLazy :: SearchTree r -> IO [r]
fdfsBagLazy = (STMBag.lazyTaskBag Queue $ Just STMBag.takeFirst) . (:[]) . dfsTask

dfsTask :: SearchTree a -> TaskIO a (Maybe a)
dfsTask None         = return   Nothing
dfsTask (One v)      = return $ Just v
dfsTask (Choice l r) = do
  addTask $ dfsTask r
  dfsTask l

bfsBag :: MonadIO m => SearchTree r -> BagT r m a -> m a
bfsBag = (STMBag.runTaskBag Queue $ Just STMBag.takeFirst) . (:[]) . bfsTask

bfsBagLazy :: SearchTree r -> IO [r]
bfsBagLazy = (STMBag.lazyTaskBag Queue $ Just STMBag.takeFirst) . (:[]) . bfsTask

bfsTask None         = NoResult
bfsTask (One v)      = OneResult v
bfsTask (Choice l r) = AddInterruptibles [bfsTask l, bfsTask r]

fairBag :: MonadIO m => SearchTree r -> BagT r m a -> m a
fairBag = (STMBag.runInterruptingBag Queue $ Just STMBag.takeFirst) . (:[]) . bfsTask

fairBagLazy :: SearchTree r -> IO [r]
fairBagLazy = (STMBag.lazyInterruptingBag Queue $ Just STMBag.takeFirst) . (:[]) . bfsTask

-- | Parallel Breadth-first search
--   This is the /bfsParallel2/ in: Integration of
--   Parallel and Fair Search Strategies
--   for Non-Deterministic Programs
--   into the Curry System KiCS2
bfsParallel :: SearchTree a -> [a]
bfsParallel t =
  bfsSearch (t `using` parTree)

parTree :: SearchTree a -> Eval (SearchTree a)
parTree (Choice l r) = do
  r'  <-  (rpar `dot` parTree)  r
  l'  <-  (rpar `dot` parTree)  l
  return (Choice l' r')
parTree  t           = r0 t

--   This is the /splitAll1/ in: Integration of
--   Parallel and Fair Search Strategies
--   for Non-Deterministic Programs
--   into the Curry System KiCS2
splitAll :: SearchTree a -> [a]
splitAll None         = []
splitAll (One x)      = [x]
splitAll (Choice l r) = runEval $ do
  rs <- rpar $ splitAll r
  ls <- rseq $ splitAll l
  return $ ls ++ rs

data ExecutionState = Stopped
                 -- number of threads left / running threads
                  | Executing Int [ThreadId]

data ThreadResult a = ThreadStopped ThreadId
                    | Value         a

-- | A parallel search implemented with concurrent Haskell
conSearch :: Int         -- ^ The maximum number of threads to use
         -> SearchTree a -- ^ The search tree to search in
         -> IO [a]
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
  unsafeInterleaveIO $ handleResults dummyRef threadVar resultChan
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

  handleResults :: IORef () -> MVar ExecutionState -> Chan (ThreadResult a) -> IO [a]
  handleResults dummyRef threadVar resultChan = do
     -- this is for the optimizer not to optimize out the dummyRef
    result <- readChan resultChan
    case result of
      ThreadStopped tid -> do
        state <- modifyMVar threadVar $ return . (\a -> (a,a)) . (removeThread tid)
        case state of
          Executing _ (_:_) ->
            handleResults dummyRef threadVar resultChan
          _ -> do
            writeIORef dummyRef ()
            return []
      Value a  -> do
        as <- unsafeInterleaveIO $ handleResults dummyRef threadVar resultChan
        return $ a:as

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

data Stop = Stop
  deriving (Typeable, Show)

instance Exception Stop

-- | Fair search strategy.
--   This is the /fairSearch2/ in: Integration of
--   Parallel and Fair Search Strategies
--   for Non-Deterministic Programs
--   into the Curry System KiCS2
fairSearch :: SearchTree a -> IO [a]
fairSearch tree = do
  chan  <- newChan
  root  <- uninterruptibleMask_ $ forkIOWithUnmask (\unmask -> searchThread unmask chan tree)
  dummy <- newIORef ()
  _     <- mkWeakIORef dummy $ void $ forkIO $ throwTo root Stop
  handleResults chan dummy
 where
  handleResults chan dummy = do
    ans <- readChan chan
    case ans of
      ThreadStopped _ -> do
        writeIORef dummy ()
        return []
      Value v         -> do
        vs <- unsafeInterleaveIO $ handleResults chan dummy
        return $ v:vs

  fin parent = do
    tid <- myThreadId
    writeChan parent $ ThreadStopped tid

  searchThread :: (forall a. IO a -> IO a) -> Chan (ThreadResult b) -> SearchTree b -> IO ()
  searchThread unmask parent tree = do
    tree' <- unmask (evaluate tree) `catch` (\Stop -> return None)
    case tree' of
      None  ->
        fin parent
      One v -> do
        writeChan parent $ Value v
        fin parent
      Choice l r -> do
        chan <- newChan
        t1   <- forkIO $ searchThread unmask chan l
        t2   <- forkIO $ searchThread unmask chan r
        listenChan unmask [t1,t2] chan parent
   where
    listenChan _       []  _    parent = do
      fin parent
    listenChan unmask ts chan parent = do
      ans <- unmask (liftM Just $ readChan chan) `catch` (\Stop -> do
        mapM_ (\t -> throwTo t Stop) ts
        return Nothing)
      case ans of
        Just (ThreadStopped t) -> listenChan unmask (delete t ts) chan parent
        Just value             -> do
          writeChan parent value
          listenChan unmask ts chan parent
        Nothing                -> return ()

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
