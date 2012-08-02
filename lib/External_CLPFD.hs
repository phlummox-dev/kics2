import qualified Curry_Prelude as CP

import FDData

-- External implementations for constraint functions:
-- (curry list arguments have to be converted to haskell lists using toFDList)

external_d_C_prim_domain :: CP.OP_List CP.C_Int -> CP.C_Int -> CP.C_Int -> CP.OP_List CP.C_Int -> ConstStore -> CP.C_Success
external_d_C_prim_domain vs@(CP.Choices_OP_List _ i@(FreeID _ _) _) l u _ cs = ((\vs1 _ -> domain vs1 l u i) $!! (CP.d_C_ensureSpine vs cs)) cs
external_d_C_prim_domain vs l u (CP.Choices_OP_List _ i@(FreeID _ _) _) cs = ((\vs1 _ -> domain vs1 l u i) $!! (CP.d_C_ensureSpine vs cs)) cs

domain :: CP.OP_List CP.C_Int -> CP.C_Int -> CP.C_Int -> ID -> CP.C_Success
domain vs l u i =
  let c = wrapCs $ FDDomain (toFDList i vs) (toCsExpr l) (toCsExpr u)
  in guardCons defCover (WrappedConstr [c]) C_Success

external_d_C_prim_FD_plus :: CP.C_Int -> CP.C_Int -> CP.C_Int -> ConstStore -> CP.C_Int
external_d_C_prim_FD_plus x y res cs | gnfCheck x && gnfCheck y = CP.d_OP_plus x y cs
                                     | otherwise                = let c = wrapCs $ newArithConstr Plus x y res
                                                                  in guardCons defCover (WrappedConstr [c]) res

external_d_C_prim_FD_minus :: CP.C_Int -> CP.C_Int -> CP.C_Int -> ConstStore -> CP.C_Int
external_d_C_prim_FD_minus x y res cs | gnfCheck x && gnfCheck y = CP.d_OP_minus x y cs
                                      | otherwise                = let c = wrapCs $ newArithConstr Minus x y res
                                                                   in guardCons defCover (WrappedConstr [c]) res

external_d_C_prim_FD_times :: CP.C_Int -> CP.C_Int -> CP.C_Int -> ConstStore -> CP.C_Int
external_d_C_prim_FD_times x y res cs | gnfCheck x && gnfCheck y = CP.d_OP_star x y cs
                                      | otherwise                = let c = wrapCs $ newArithConstr Mult x y res
                                                                   in guardCons defCover (WrappedConstr [c]) res

external_d_C_prim_FD_equal :: CP.C_Int -> CP.C_Int -> ConstStore -> CP.C_Success
external_d_C_prim_FD_equal x y cs | gnfCheck x && gnfCheck y = if xEqualY then C_Success else CP.d_C_failed cs
                                  | otherwise                = let c = wrapCs $ newRelConstr Equal x y
                                                               in guardCons defCover (WrappedConstr [c]) C_Success
  where xEqualY = fromCurry $ CP.d_OP_eq_eq x y cs

external_d_C_prim_FD_notequal :: CP.C_Int -> CP.C_Int -> ConstStore -> CP.C_Success
external_d_C_prim_FD_notequal x y cs | gnfCheck x && gnfCheck y = if xNotEqualY then C_Success else CP.d_C_failed cs
                                     | otherwise                = let c = wrapCs $ newRelConstr Diff x y
                                                                  in guardCons defCover (WrappedConstr [c]) C_Success
  where xNotEqualY = fromCurry $ CP.d_C_not (CP.d_OP_eq_eq x y cs) cs

external_d_C_prim_FD_le :: CP.C_Int -> CP.C_Int -> ConstStore -> CP.C_Success
external_d_C_prim_FD_le x y cs | gnfCheck x && gnfCheck y = if xLessY then C_Success else CP.d_C_failed cs
                               | otherwise                = let c = wrapCs $ newRelConstr Less x y
                                                            in guardCons defCover (WrappedConstr [c]) C_Success
  where xLessY = fromCurry $ CP.d_OP_lt_eq x (CP.d_OP_minus y (CP.C_Int 1#) cs) cs

external_d_C_prim_FD_leq :: CP.C_Int -> CP.C_Int -> ConstStore -> CP.C_Success
external_d_C_prim_FD_leq x y cs | gnfCheck x && gnfCheck y = if xLessEqualY then C_Success else CP.d_C_failed cs
                                | otherwise                = let c = wrapCs $ newRelConstr LessEqual x y
                                                             in guardCons defCover (WrappedConstr [c]) C_Success
  where xLessEqualY = fromCurry $ CP.d_OP_lt_eq x y cs

external_d_C_prim_FD_ge :: CP.C_Int -> CP.C_Int -> ConstStore -> CP.C_Success
external_d_C_prim_FD_ge x y cs = d_C_prim_FD_le y x cs

external_d_C_prim_FD_geq :: CP.C_Int -> CP.C_Int -> ConstStore -> CP.C_Success
external_d_C_prim_FD_geq x y cs = d_C_prim_FD_leq y x cs

external_d_C_prim_allDifferent :: CP.OP_List CP.C_Int -> CP.OP_List CP.C_Int -> ConstStore -> CP.C_Success
external_d_C_prim_allDifferent vs@(CP.Choices_OP_List _ i@(FreeID _ _) _) _ cs = ((\vs1 cs1 -> allDifferent vs1 i cs1) $!! (CP.d_C_ensureSpine vs cs)) cs
external_d_C_prim_allDifferent vs (CP.Choices_OP_List _ i@(FreeID _ _) _) cs = ((\vs1 cs1 -> allDifferent vs1 i cs1) $!! (CP.d_C_ensureSpine vs cs)) cs

allDifferent :: CP.OP_List CP.C_Int -> ID -> ConstStore -> CP.C_Success
allDifferent vs i cs 
  | gnfCheck vs = if allDiff (fromCurry vs) then C_Success else CP.d_C_failed cs
  | otherwise   = let c = wrapCs $ FDAllDifferent (toFDList i vs)
                  in guardCons defCover (WrappedConstr [c]) C_Success

allDiff :: [Int] -> Bool
allDiff []     = True
allDiff (v:vs) = all (/= v) vs && allDiff vs

external_d_C_prim_sum :: CP.OP_List CP.C_Int -> CP.C_Int -> CP.OP_List CP.C_Int -> ConstStore -> CP.C_Int
external_d_C_prim_sum vs@(CP.Choices_OP_List _ i@(FreeID _ _) _) res _ cs = ((\vs1 _ -> sumList vs1 res i) $!! (CP.d_C_ensureSpine vs cs)) cs
external_d_C_prim_sum vs res (CP.Choices_OP_List _ i@(FreeID _ _) _) cs = ((\vs1 _ -> sumList vs1 res i) $!! (CP.d_C_ensureSpine vs cs)) cs

sumList :: CP.OP_List CP.C_Int -> CP.C_Int -> ID -> CP.C_Int
sumList vs res i
  | gnfCheck vs = toCurry (Prelude.sum (fromCurry vs :: [Int]))
  | otherwise   = let c = wrapCs $ FDSum (toFDList i vs) (toCsExpr res)
                  in guardCons defCover (WrappedConstr [c]) res

external_d_C_prim_labelingWith :: C_LabelingStrategy -> CP.OP_List CP.C_Int -> CP.OP_List CP.C_Int -> CP.OP_List CP.C_Int -> ConstStore -> CP.C_Success
external_d_C_prim_labelingWith strategy vs@(CP.Choices_OP_List _ i@(FreeID _ _) _) _ (CP.Choices_OP_List _ j@(FreeID _ _) _) cs = ((\vs1 _ -> labeling strategy vs1 j i) $!! (CP.d_C_ensureSpine vs cs)) cs
external_d_C_prim_labelingWith strategy vs (CP.Choices_OP_List _ i@(FreeID _ _) _) (CP.Choices_OP_List _ j@(FreeID _ _) _) cs = ((\vs1 _ -> labeling strategy vs1 j i) $!! (CP.d_C_ensureSpine vs cs)) cs

labeling :: C_LabelingStrategy -> CP.OP_List CP.C_Int -> ID -> ID -> CP.C_Success
labeling strategy vs j i =
  let c = wrapCs $ FDLabeling (fromCurry strategy) (toFDList i vs) j
  in guardCons defCover (WrappedConstr [c]) C_Success

newArithConstr :: ArithOp -> CP.C_Int -> CP.C_Int -> CP.C_Int -> FDConstraint
newArithConstr arithOp x y result = FDArith arithOp (toCsExpr x) (toCsExpr y) (toCsExpr result)

newRelConstr :: RelOp -> CP.C_Int -> CP.C_Int -> FDConstraint
newRelConstr relOp x y = FDRel relOp (toCsExpr x) (toCsExpr y)


-- Conversion between Curry-LabelingStrategy and Haskell-LabelingStrategy

instance ConvertCurryHaskell C_LabelingStrategy LabelingStrategy where
  toCurry InOrder   = C_InOrder
  toCurry FirstFail = C_FirstFail
  toCurry MiddleOut = C_MiddleOut
  toCurry EndsOut   = C_EndsOut

  fromCurry C_InOrder   = InOrder
  fromCurry C_FirstFail = FirstFail
  fromCurry C_MiddleOut = MiddleOut
  fromCurry C_EndsOut   = EndsOut
  fromCurry _           = error "KiCS2 error: LabelingStrategy data with no ground term"

-- helper function to convert curry integer lists to lists of fd terms
toFDList :: Constrainable a b => ID -> CP.OP_List a -> FDList b
toFDList i vs = FDList i (toFDList' vs)
  where
   toFDList' CP.OP_List        = []
   toFDList' (CP.OP_Cons v vs) = toCsExpr v : toFDList' vs

-- Typeclass for checking whether non-det value is in ground normal form or not
class NonDet a => GNFChecker a where
  gnfCheck :: a -> Bool
  gnfCheck x = gnfCheck' (try x)
    where gnfCheck' (Val _) = True
          gnfCheck' _       = False

instance GNFChecker CP.C_Int

instance GNFChecker a => GNFChecker (CP.OP_List a) where
  gnfCheck CP.OP_List = True
  gnfCheck (CP.OP_Cons x xs) = gnfCheck x && gnfCheck xs
