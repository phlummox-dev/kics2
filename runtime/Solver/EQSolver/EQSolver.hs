{-# LANGUAGE TypeFamilies #-}

module Solver.EQSolver.EQSolver where

import Solver.Interface
import Solver.EQSolver.EQDomain (ToDomain (..))
import Solver.EQSolver.OvertonEQ
import Types

data EQSolver = EQSolver

eqSolver :: EQSolver
eqSolver = EQSolver

instance IncrementalSolver EQSolver where
  type ForConstraint EQSolver = EQConstraints

  processWith = processEQConstr
  
processEQConstr :: (Store m, NonDet a) => EQSolver -> Cover -> EQConstraints -> a -> Solution m a
processEQConstr _ = solve

(>>-) :: Monad m => Solution m a -> (a -> Solution m b) -> Solution m b
ma >>- mbf = do
  mbSolutionA <- ma
  case mbSolutionA of
    Nothing          -> noSolution
    Just (resetA, a) -> do
      mbSolutionB <- mbf a
      case mbSolutionB of
        Nothing          -> resetA >> noSolution
        Just (resetB, b) -> return $ Just (resetB >> resetA, b)
  
solve :: (Store m, NonDet a) => Cover -> EQConstraints -> a -> Solution m a
solve cd cnstrs val = solve' (getEQConstrList cnstrs) val
 where
  solve' []     a = mkSolution a
  solve' (c:cs) a = solveOne cd c a >>- solve' cs
  
solveOne :: (Store m, NonDet a) => Cover -> EQConstraint -> a -> Solution m a
solveOne _ (EQUnsolvable _) _ = noSolution
solveOne cd (EQConstraintChoice d i lcs rcs) e = lookupDom i >>= follow
  where
  follow [0]   = mkSolution $ mkGuardExt cd (EQStructConstr lcs) e
  follow [1]   = mkSolution $ mkGuardExt cd (EQStructConstr rcs) e
  follow (_:_) = mkSolution $ choiceCons d i
                                    (mkGuardExt cd (EQStructConstr lcs) e)
                                    (mkGuardExt cd (EQStructConstr rcs) e)
  follow c           = error $ "Solver.solve.choose: CC:" ++ show c

solveOne cd cc@(EQConstraintChoices d i css) e = lookupDom i >>= follow
  where
  follow [c]   = mkSolution $ mkGuardExt cd (EQStructConstr (css !! c)) e
  follow (_:_) = mkSolution $ choicesCons d i
                                    $ map (\cs -> mkGuardExt cd (EQStructConstr cs) e) css
--  follow (LazyBind cs) = mkSolution $ mkGuardExt cd (StructConstr (cs ++ [cc])) e
  follow c             = error $ "Solver.solve.choose: CCs:" ++ show c

solveOne cd c@(EQRel _ _ _ _) e = do
  store <- getEQStore
  let c' = processConstr c
  case updateEQ c' store of
    [store'] -> do let reset = setEQStore store
                   setEQStore store'
                   return $ Just (reset,e)
    _        -> noSolution

processConstr :: EQConstraint -> OvertonEQ ()
processConstr (EQRel op x (BindTo y) _) = do
  let constraint = matchRelOp op
  constraint x y
processConstr (EQRel op i (ChooseN c p) _) = hasValue i (c,p)

matchRelOp :: EQOp -> BinaryConstraint
matchRelOp EQEqual = same

-- Replacement for lookupDecision
lookupDom :: Store m => ID -> m [Int]
lookupDom i = do
  store <- getEQStore
  let labelAction = labelling [i]
  return $ concat $ runEQ labelAction store

-- Replacement for setDecision
setDom :: (Store m, ToDomain a) => ID -> a -> m ()
setDom i d = do
  store <- getEQStore
  let dom          = toDomain d
      updateAction = do args <- lookupArgs i 
                        let eqd = EQDomain dom args
                        updateEQDomain i eqd
  case updateEQ updateAction store of
       [store'] -> setEQStore store'
       _        -> error "Constraint store is in invalid state"

