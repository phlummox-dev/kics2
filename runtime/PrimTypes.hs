{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, MagicHash #-}
module PrimTypes where

import System.IO (Handle)
import GHC.Exts (Int (I#), Int#, (==#), (<#), (*#), (+#), quotInt#, remInt#, negateInt#)

import Debug
import Types

-- Curry_Int
-- BEGIN GENERATED FROM PrimTypes.curry
data C_Int
     = C_Int Int#
     | C_CurryInt BinInt
     | Choice_C_Int Cover ID C_Int C_Int
     | Choices_C_Int Cover ID ([C_Int])
     | Fail_C_Int Cover FailInfo
     | Guard_C_Int Cover WrappedConstraint C_Int

instance Show C_Int where
  showsPrec d (Choice_C_Int cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Int cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Int cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Int _ _) = showChar '!'
  showsPrec d (C_Int x1) = shows (I# x1)
  showsPrec d (C_CurryInt x1) = case ((\x _ _ -> x) $## x1) (error "Show C_Int: nesting depth used") emptyCs of
    Choice_BinInt _ _ _ _ -> shows x1
    Choices_BinInt _ _ _  -> shows x1
    Fail_BinInt _ _       -> shows x1
    Guard_BinInt _ _ _    -> shows x1
    gnfBinInt             -> shows (I# (curryint2primint gnfBinInt))

instance Read C_Int where
  readsPrec d s = map readInt (readsPrec d s) where readInt (I# i, s) = (C_Int i, s)

instance NonDet C_Int where
  choiceCons = Choice_C_Int
  choicesCons = Choices_C_Int
  failCons = Fail_C_Int
  guardCons = Guard_C_Int
  try (Choice_C_Int cd i x y) = tryChoice cd i x y
  try (Choices_C_Int cd i xs) = tryChoices cd i xs
  try (Fail_C_Int cd info) = Fail cd info
  try (Guard_C_Int cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Int cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Int cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Int cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Int _  i _) = error ("Prelude.Int.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Int cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Int cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Int where
  generate s cd = Choices_C_Int cd (freeID [1] s) [C_CurryInt (generate (leftSupply s) cd)]

instance NormalForm C_Int where
  ($!!) cont x@(C_Int _) cd cs = cont x cd cs
  ($!!) cont (C_CurryInt x1) cd cs = ((\y1 -> cont (C_CurryInt y1)) $!! x1) cd cs
  ($!!) cont (Choice_C_Int d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_C_Int d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_C_Int d c x) cd cs = guardCons d c ((cont $!! x) cd $! (addCs c cs))
  ($!!) _ (Fail_C_Int cd info) _ _ = failCons cd info
  ($##) cont x@(C_Int _) cd cs = cont x cd cs
  ($##) cont (C_CurryInt x1) cd cs = ((\y1 -> cont (C_CurryInt y1)) $## x1) cd cs
  ($##) cont (Choice_C_Int d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_C_Int d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_C_Int d c x) cd cs = guardCons d c ((cont $## x) cd $! (addCs c cs))
  ($##) _ (Fail_C_Int d info) _ _ = failCons d info
  searchNF search cont x@(C_Int _) = cont x
  searchNF search cont (C_CurryInt x1) = search (\y1 -> cont (C_CurryInt y1)) x1
  searchNF _ _ x = error ("Prelude.Int.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Int where
  (=.=) (C_Int      x1) (C_Int      y1) cd _  = if (x1 ==# y1) then C_Success else Fail_C_Success cd defFailInfo
  (=.=) (C_Int      x1) (C_CurryInt y1) cd cs = ((primint2curryint x1) =:=# y1) cd cs
  (=.=) (C_CurryInt x1) (C_Int      y1) cd cs = (x1 =:= (primint2curryint y1)) cd cs
  (=.=) (C_CurryInt x1) (C_CurryInt y1) cd cs = (x1 =:= y1) cd cs
  (=.=) _               _               cd _  = Fail_C_Success cd defFailInfo
  (=.<=) (C_Int      x1) (C_Int      y1) cd _ = if (x1 ==# y1) then C_Success else Fail_C_Success cd defFailInfo
  (=.<=) (C_Int      x1) (C_CurryInt y1) cd cs = ((primint2curryint x1) =:<= y1) cd cs
  (=.<=) (C_CurryInt x1) (C_Int      y1) cd cs = (x1 =:<= (primint2curryint y1)) cd cs
  (=.<=) (C_CurryInt x1) (C_CurryInt y1) cd cs = (x1 =:<= y1) cd cs
  (=.<=) _ _ cd _= Fail_C_Success cd defFailInfo
  bind cd i (C_Int      x2) = (i :=: ChooseN 0 1) : bind cd (leftID i) (primint2curryint x2)
  bind cd i (C_CurryInt x2) = (i :=: ChooseN 0 1) : bind cd (leftID i) x2
  bind cd i (Choice_C_Int d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_Int d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_Int d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ c@(Choices_C_Int _ i@(ChoiceID _) _) = error ("Prelude.Int.bind: Choices with ChoiceID: " ++ (show c))
  bind _ _ (Fail_C_Int _ info) = [Unsolvable info]
  bind cd  i (Guard_C_Int _ c e) = case unwrapCs c of
    Just cs -> getConstrList cs ++ (bind cd i e)
    Nothing -> error "Prelude.Int.bind: Called bind with a guard expression containing a non-equation constraint"
  lazyBind cd i (C_Int      x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) (primint2curryint x2))]
  lazyBind cd i (C_CurryInt x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) x2)]
  lazyBind cd i (Choice_C_Int d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_Int d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_Int d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ c@(Choices_C_Int _ i@(ChoiceID _) _) = error ("Prelude.Int.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _  _ (Fail_C_Int _ info) = [Unsolvable info]
  lazyBind cd i (Guard_C_Int _ c e) = case unwrapCs c of
    Just cs -> getConstrList cs ++ [(i :=: (LazyBind (lazyBind cd i e)))]
    Nothing -> error "Prelude.Int.lazyBind: Called lazyBind with a guard expression containing a non-equation constraint"
  fromDecision cd i (ChooseN 0 1) = 
    do
     x3 <- lookupValue cd (leftID i)
     if (isFree x3)
         then (return (generate (supply i) cd))
         else (return (C_CurryInt x3))
  fromDecision cd i NoDecision   = return (generate (supply i) cd)
  fromDecision _  i ChooseLeft   = error ("Prelude.Int.fromDecision: ChooseLeft decision for free ID: " ++ (show i))
  fromDecision _  i ChooseRight  = error ("Prelude.Int.fromDecision: ChooseRight decision for free ID: " ++ (show i))
  fromDecision _  _ (LazyBind _) = error "Prelude.Int.fromDecision: No rule for LazyBind decision yet"
  bindEQ cd op i (C_Int      x2) = (EQRel op i (ChooseN 0 1) 1) : bindEQ cd op (leftID i) (primint2curryint x2)
  bindEQ cd op i (C_CurryInt x2) = (EQRel op i (ChooseN 0 1) 1) : bindEQ cd op (leftID i) x2
  bindEQ cd op i (Choice_C_Int d j l r) = [(EQConstraintChoice d j (bindEQ cd op i l) (bindEQ cd op i r))]
  bindEQ cd op i (Choices_C_Int d j@(FreeID _ _) xs) = bindOrNarrowEQ cd op i d j xs
  bindEQ cd op i (Choices_C_Int d j@(NarrowedID _ _) xs) = [(EQConstraintChoices d j (map (bindEQ cd op i) xs))]
  bindEQ _  _  _ c@(Choices_C_Int _ i@(ChoiceID _) _) = error ("Prelude.Int.bindEQ: Choices with ChoiceID: " ++ (show c))
  bindEQ _  _  _ (Fail_C_Int _ info) = [EQUnsolvable info]
  bindEQ cd op i (Guard_C_Int _ c e) = case unwrapCs c of
    Just cs -> getEQConstrList cs ++ (bindEQ cd op i e)
    Nothing -> error "Prelude.Int.bindEQ: Called bindEQ with a guard expression containing a non-equation constraint"

instance ConvertCurryHaskell C_Int Int where
  toCurry (I# i) = C_Int i

  fromCurry (C_Int i)      = I# i
  fromCurry (C_CurryInt i) = I# (curryint2primint i)
  fromCurry _              = error "KiCS2 error: Int data with no ground term"

instance ConvertCurryHaskell C_Int Integer where
  toCurry i = int2C_Int (fromInteger i)
   where
    int2C_Int (I# c) = C_Int c

  fromCurry (C_Int      i) = toInteger (I# i)
  fromCurry (C_CurryInt i) = toInteger (I# (curryint2primint i))
  fromCurry _              = error "KiCS2 error: Int data with no ground term"

primint2curryint :: Int# -> BinInt
primint2curryint n
  | n <#  0#  = Neg (primint2currynat (negateInt# n))
  | n ==# 0#  = Zero
  | otherwise = Pos (primint2currynat n)

primint2currynat :: Int# -> Nat
primint2currynat n
  | n ==# 1#                = IHi
  | (n `remInt#` 2#) ==# 0# = O (primint2currynat (n `quotInt#` 2#))
  | otherwise               = I (primint2currynat (n `quotInt#` 2#))

currynat2primint :: Nat -> Int#
currynat2primint IHi   = 1#
currynat2primint (O n) = 2# *# currynat2primint n
currynat2primint (I n) = 2# *# currynat2primint n +# 1#
currynat2primint _ = error "KiCS2 error: Prelude.currynat2primint: no ground term"

curryint2primint :: BinInt -> Int#
curryint2primint Zero    = 0#
curryint2primint (Pos n) = currynat2primint n
curryint2primint (Neg n) = negateInt# (currynat2primint n)
curryint2primint _ = error "KiCS2 error: Prelude.curryint2primint: no ground term"

-- ---------------------------------------------------------------------------
-- Conversion to constrainable term types
-- ---------------------------------------------------------------------------

instance Constrainable C_Int (Term Int) where
  toCsExpr x@(C_Int _)                        = Const (fromCurry x) 
  toCsExpr x@(C_CurryInt _)                   = Const (fromCurry x)
  toCsExpr (Choices_C_Int _ i@(FreeID _ _) _) = Var i
  toCsExpr _                                  = error "toCsExpr: Cannot convert non-normalized int data"

  updateTerm cd (Var i) = do a <- lookupValue cd i
                             return (toCsExpr a)
  updateTerm _  c       = return c

-- BinInt

data BinInt
     = Neg Nat
     | Zero
     | Pos Nat
     | Choice_BinInt Cover ID BinInt BinInt
     | Choices_BinInt Cover ID ([BinInt])
     | Fail_BinInt Cover FailInfo
     | Guard_BinInt Cover (WrappedConstraint) BinInt

instance Show BinInt where
  showsPrec d (Choice_BinInt cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_BinInt cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_BinInt cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_BinInt _ _) = showChar '!'
  showsPrec _ (Neg x1) = (showString "(Neg") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ Zero = showString "Zero"
  showsPrec _ (Pos x1) = (showString "(Pos") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read BinInt where
  readsPrec d s = (readParen (d > 10) (\r -> [ (Neg x1,r1) | (_,r0) <- readQualified "Prelude" "Neg" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen False (\r -> [ (Zero,r0) | (_,r0) <- readQualified "Prelude" "Zero" r]) s) ++ (readParen (d > 10) (\r -> [ (Pos x1,r1) | (_,r0) <- readQualified "Prelude" "Pos" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet BinInt where
  choiceCons = Choice_BinInt
  choicesCons = Choices_BinInt
  failCons = Fail_BinInt
  guardCons = Guard_BinInt
  try (Choice_BinInt cd i x y) = tryChoice cd i x y
  try (Choices_BinInt cd i xs) = tryChoices cd i xs
  try (Fail_BinInt cd info) = Fail cd info
  try (Guard_BinInt cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_BinInt  cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_BinInt cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_BinInt cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_BinInt _ i _) = internalError ("Prelude.BinInt.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_BinInt cd info) = f cd info
  match _ _ _ _ f _ (Guard_BinInt cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x


instance Generable BinInt where
  generate s cd = Choices_BinInt cd (freeID [1,0,1] s) [(Neg (generate (leftSupply s) cd )),Zero,(Pos (generate (leftSupply s) cd))]


instance NormalForm BinInt where
  ($!!) cont (Neg x1) cd cs = ((\y1 cd1 cs1 -> cont (Neg y1) cd1 cs1) $!! x1) cd cs
  ($!!) cont Zero cd cs = cont Zero cd cs
  ($!!) cont (Pos x1) cd cs = ((\y1 cd1 cs1 -> cont (Pos y1) cd1 cs1) $!! x1) cd cs
  ($!!) cont (Choice_BinInt d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_BinInt d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_BinInt d c x) cd cs = guardCons d c ((cont $!! x) cd $! addCs c cs)
  ($!!) _ (Fail_BinInt d info) _ _ = failCons d info
  ($##) cont (Neg x1) cd cs = ((\y1 cd1 cs1 -> cont (Neg y1) cd1 cs1) $## x1) cd cs
  ($##) cont Zero cd cs = cont Zero cd cs
  ($##) cont (Pos x1) cd cs = ((\y1 cd1 cs1 -> cont (Pos y1) cd1 cs1) $## x1)cd  cs
  ($##) cont (Choice_BinInt d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_BinInt d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_BinInt d c x) cd cs = guardCons d c ((cont $## x) cd $! addCs c cs)
  ($##) _ (Fail_BinInt d info) _ _ = failCons d info
  searchNF search cont (Neg x1) = search (\y1 -> cont (Neg y1)) x1
  searchNF _ cont Zero = cont Zero
  searchNF search cont (Pos x1) = search (\y1 -> cont (Pos y1)) x1
  searchNF _ _ x = internalError ("PrimTypes.BinInt.searchNF: no constructor: " ++ (show x))


instance Unifiable BinInt where
  (=.=) (Neg x1) (Neg y1) cd cs = (x1 =:=# y1) cd cs
  (=.=) Zero Zero _ _ = C_Success
  (=.=) (Pos x1) (Pos y1) cd cs = (x1 =:=# y1) cd cs
  (=.=) _ _ cd _ = Fail_C_Success cd defFailInfo
  (=.<=) (Neg x1) (Neg y1) cd cs = (x1 =:<= y1) cd cs
  (=.<=) Zero Zero _ _ = C_Success
  (=.<=) (Pos x1) (Pos y1) cd cs = (x1 =:<= y1) cd cs
  (=.<=) _ _ cd _= Fail_C_Success cd  defFailInfo
  bind cd i (Neg x2) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x2)]))
  bind _  i Zero = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i (Pos x2) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x2)]))
  bind cd i (Choice_BinInt d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_BinInt d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs 
  bind cd i (Choices_BinInt d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ (Choices_BinInt _ i@(ChoiceID _) _) = internalError ("Prelude.BinInt.bind: Choices with ChoiceID: " ++ (show i))
  bind _  _ (Fail_BinInt cd info) = [Unsolvable info]
  bind cd i (Guard_BinInt _ c e) = case unwrapCs c of
    Just cs -> (getConstrList cs) ++ (bind cd i e)
    Nothing -> error "Prelude.BinInt.bind: Called bind with a guard expression containing a non-equation constraint"
  lazyBind cd i (Neg x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x2)))]
  lazyBind _  i Zero = [(i :=: (ChooseN 1 0))]
  lazyBind cd i (Pos x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x2)))]
  lazyBind cd i (Choice_BinInt d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_BinInt d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_BinInt d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ (Choices_BinInt _ i@(ChoiceID _) _) = internalError ("Prelude.BinInt.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _  _ (Fail_BinInt _ info) = [Unsolvable info]
  lazyBind cd i (Guard_BinInt _ c e) = case unwrapCs c of
    Just cs -> (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind cd i e)))]
    Nothing -> error "Prelude.BinInt.lazyBind: Called lazyBind with a guard expression containing a non-equation constraint"
  fromDecision cd i (ChooseN 0 1) = 
    do
     x3 <- lookupValue cd (leftID i)
     if (isFree x3)
         then (return (generate (supply i) cd))
         else (return (Neg x3))
  fromDecision _  _ (ChooseN 1 0) = return Zero
  fromDecision cd i (ChooseN 2 1) = 
    do
     x3 <- lookupValue cd (leftID i)
     if (isFree x3)
         then (return (generate (supply i) cd))
         else (return (Pos x3))
  fromDecision cd i NoDecision   = return (generate (supply i) cd)
  fromDecision _  i ChooseLeft   = error ("Prelude.BinInt.fromDecision: ChooseLeft decision for free ID: " ++ (show i))
  fromDecision _  i ChooseRight  = error ("Prelude.BinInt.fromDecision: ChooseRight decision for free ID: " ++ (show i))
  fromDecision _  _ (LazyBind _) = error "Prelude.BinInt.fromDecision: No rule for LazyBind decision yet"
  bindEQ cd op i (Neg x2) = ((EQRel op i (ChooseN 0 1) 3):(concat [(bindEQ cd op (leftID i) x2)]))
  bindEQ _  op i Zero = ((EQRel op i (ChooseN 1 0) 3):(concat []))
  bindEQ cd op i (Pos x2) = ((EQRel op i (ChooseN 2 1) 3):(concat [(bindEQ cd op (leftID i) x2)]))
  bindEQ cd op i (Choice_BinInt d j l r) = [(EQConstraintChoice d j (bindEQ cd op i l) (bindEQ cd op i r))]
  bindEQ cd op i (Choices_BinInt d j@(FreeID _ _) xs) = bindOrNarrowEQ cd op i d j xs 
  bindEQ cd op i (Choices_BinInt d j@(NarrowedID _ _) xs) = [(EQConstraintChoices d j (map (bindEQ cd op i) xs))]
  bindEQ _  _  _ (Choices_BinInt _ i@(ChoiceID _) _) = internalError ("Prelude.BinInt.bindEQ: Choices with ChoiceID: " ++ (show i))
  bindEQ _  _  _ (Fail_BinInt cd info) = [EQUnsolvable info]
  bindEQ cd op i (Guard_BinInt _ c e) = case unwrapCs c of
    Just cs -> (getEQConstrList cs) ++ (bindEQ cd op i e)
    Nothing -> error "Prelude.BinInt.bindEQ: Called bindEQ with a guard expression containing a non-equation constraint"
-- Nats

data Nat
     = IHi
     | O Nat
     | I Nat
     | Choice_Nat Cover ID Nat Nat
     | Choices_Nat Cover ID ([Nat])
     | Fail_Nat Cover  FailInfo
     | Guard_Nat Cover (WrappedConstraint) Nat

instance Show Nat where
  showsPrec d (Choice_Nat cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_Nat cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_Nat cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_Nat _ _) = showChar '!'
  showsPrec _ IHi = showString "IHi"
  showsPrec _ (O x1) = (showString "(O") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (I x1) = (showString "(I") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read Nat where
  readsPrec d s = (readParen False (\r -> [ (IHi,r0) | (_,r0) <- readQualified "Prelude" "IHi" r]) s) ++ ((readParen (d > 10) (\r -> [ (O x1,r1) | (_,r0) <- readQualified "Prelude" "O" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (I x1,r1) | (_,r0) <- readQualified "Prelude" "I" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet Nat where
  choiceCons = Choice_Nat
  choicesCons = Choices_Nat
  failCons = Fail_Nat
  guardCons = Guard_Nat
  try (Choice_Nat cd i x y) = tryChoice cd i x y
  try (Choices_Nat cd i xs) = tryChoices cd i xs
  try (Fail_Nat cd info) = Fail cd info
  try (Guard_Nat cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_Nat cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_Nat cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_Nat cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_Nat cd i@(ChoiceID _) _) = internalError ("Prelude.Nat.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_Nat cd info) = f cd info
  match _ _ _ _ f _ (Guard_Nat cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x


instance Generable Nat where
  generate s cd = Choices_Nat cd (freeID [0,1,1] s) [IHi,O (generate (leftSupply s) cd),I (generate (leftSupply s) cd )]


instance NormalForm Nat where
  ($!!) cont IHi cd cs = cont IHi cd cs
  ($!!) cont (O x1) cd cs = ((\y1 cd1 cs1 -> cont (O y1) cd1 cs1) $!! x1) cd cs
  ($!!) cont (I x1) cd cs = ((\y1 cd1 cs1 -> cont (I y1) cd1 cs1) $!! x1) cd cs
  ($!!) cont (Choice_Nat d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_Nat d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_Nat d c x) cd cs = guardCons d c ((cont $!! x) cd $! addCs c cs)
  ($!!) _ (Fail_Nat d info) _ _ = failCons d info
  ($##) cont IHi cd  cs = cont IHi cd cs
  ($##) cont (O x1) cd cs = ((\y1 cd1 cs1 -> cont (O y1) cd1 cs1) $## x1) cd cs
  ($##) cont (I x1) cd cs = ((\y1 cd1 cs1 -> cont (I y1) cd1 cs1) $## x1) cd cs
  ($##) cont (Choice_Nat d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_Nat d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_Nat d c x) cd cs = guardCons d c ((cont $## x) cd $! addCs c cs)
  ($##) _ (Fail_Nat d info) _ _ = failCons d info
  searchNF _ cont IHi = cont IHi
  searchNF search cont (O x1) = search (\y1 -> cont (O y1)) x1
  searchNF search cont (I x1) = search (\y1 -> cont (I y1)) x1
  searchNF _ _ x = internalError ("PrimTypes.Nat.searchNF: no constructor: " ++ (show x))


instance Unifiable Nat where
  (=.=) IHi IHi _ _ = C_Success
  (=.=) (O x1) (O y1) cd cs = (x1 =:=# y1) cd cs
  (=.=) (I x1) (I y1) cd cs = (x1 =:=# y1) cd cs
  (=.=) _ _ cd _ = Fail_C_Success cd defFailInfo
  (=.<=) IHi IHi _ _ = C_Success
  (=.<=) (O x1) (O y1) cd cs = (x1 =:<= y1) cd cs
  (=.<=) (I x1) (I y1) cd cs = (x1 =:<= y1) cd cs
  (=.<=) _ _ cd _ = Fail_C_Success cd defFailInfo
  bind _  i IHi = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i (O x2) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x2)]))
  bind cd i (I x2) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x2)]))
  bind cd i (Choice_Nat d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_Nat d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_Nat d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ (Choices_Nat _ i@(ChoiceID _) _) = internalError ("Prelude.Nat.bind: Choices with ChoiceID: " ++ (show i))
  bind _  _ (Fail_Nat _ info) = [Unsolvable info]
  bind cd i (Guard_Nat _ c e) = case unwrapCs c of
    Just cs -> (getConstrList cs) ++ (bind cd i e)
    Nothing -> error "Prelude.Nat.bind: Called bind with a guard expression containing a non-equation constraint"
  lazyBind _  i IHi = [(i :=: (ChooseN 0 0))]
  lazyBind cd i (O x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x2)))]
  lazyBind cd i (I x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x2)))]
  lazyBind cd i (Choice_Nat d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_Nat d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_Nat d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ (Choices_Nat _ i@(ChoiceID _) _) = internalError ("Prelude.Nat.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _  _ (Fail_Nat _ info) = [Unsolvable info]
  lazyBind cd i (Guard_Nat _ c e) = case unwrapCs c of
    Just cs -> (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind cd i e)))]
    Nothing -> error "Prelude.Nat.lazyBind: Called lazyBind with a guard expression containing a non-equation constraint"
  fromDecision _  _ (ChooseN 0 0) = return IHi
  fromDecision cd i (ChooseN 1 1) = 
    do
     x3 <- lookupValue cd (leftID i)
     if (isFree x3)
         then (return (generate (supply i) cd))
         else (return (O x3))
  fromDecision cd i (ChooseN 2 1) = 
    do
     x3 <- lookupValue cd (leftID i)
     if (isFree x3)
         then (return (generate (supply i) cd))
         else (return (I x3))
  fromDecision cd i NoDecision   = return (generate (supply i) cd)
  fromDecision _  i ChooseLeft   = error ("Prelude.Nat.fromDecision: ChooseLeft decision for free ID: " ++ (show i))
  fromDecision _  i ChooseRight  = error ("Prelude.Nat.fromDecision: ChooseRight decision for free ID: " ++ (show i))
  fromDecision _  _ (LazyBind _) = error "Prelude.Nat.fromDecision: No rule for LazyBind decision yet"
  bindEQ _  op i IHi = ((EQRel op i (ChooseN 0 0) 3):(concat []))
  bindEQ cd op i (O x2) = ((EQRel op i (ChooseN 1 1) 3):(concat [(bindEQ cd op (leftID i) x2)]))
  bindEQ cd op i (I x2) = ((EQRel op i (ChooseN 2 1) 3):(concat [(bindEQ cd op (leftID i) x2)]))
  bindEQ cd op i (Choice_Nat d j l r) = [(EQConstraintChoice d j (bindEQ cd op i l) (bindEQ cd op i r))]
  bindEQ cd op i (Choices_Nat d j@(FreeID _ _) xs) = bindOrNarrowEQ cd op i d j xs
  bindEQ cd op i (Choices_Nat d j@(NarrowedID _ _) xs) = [(EQConstraintChoices d j (map (bindEQ cd op i) xs))]
  bindEQ _  _  _ (Choices_Nat _ i@(ChoiceID _) _) = internalError ("Prelude.Nat.bindEQ: Choices with ChoiceID: " ++ (show i))
  bindEQ _  _  _ (Fail_Nat _ info) = [EQUnsolvable info]
  bindEQ cd op i (Guard_Nat _ c e) = case unwrapCs c of
    Just cs -> (getEQConstrList cs) ++ (bindEQ cd op i e)
    Nothing -> error "Prelude.Nat.bindEQ: Called bindEQ with a guard expression containing a non-equation constraint"

-- Higher Order Funcs

-- BEGIN GENERATED FROM PrimTypes.curry
data Func t0 t1
     = Func (t0 -> IDSupply -> Cover -> ConstStore -> t1)
     | Choice_Func Cover ID (Func t0 t1) (Func t0 t1)
     | Choices_Func Cover ID ([Func t0 t1])
     | Fail_Func Cover FailInfo
     | Guard_Func Cover WrappedConstraint (Func t0 t1)

instance Show (Func a b) where show = internalError "ERROR: no show for Func"

instance Read (Func a b) where readsPrec = internalError "readsPrec for Func"

instance NonDet (Func t0 t1) where
  choiceCons = Choice_Func
  choicesCons = Choices_Func
  failCons = Fail_Func
  guardCons = Guard_Func
  try (Choice_Func cd i x y) = tryChoice cd i x y
  try (Choices_Func cd i xs) = tryChoices cd i xs
  try (Fail_Func cd info) = Fail cd info
  try (Guard_Func cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_Func cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_Func cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_Func cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_Func cd i _) = internalError ("Prelude.Func.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_Func cd info) = f cd info
  match _ _ _ _ f _ (Guard_Func cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable (Func a b) where generate _ = internalError "generate for Func"

instance (NormalForm t0,NormalForm t1) => NormalForm (Func t0 t1) where
  ($!!) cont f@(Func _) cd cs = cont f cd cs
  ($!!) cont (Choice_Func d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_Func d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_Func d c x) cd cs = guardCons d c ((cont $!! x) cd $! addCs c cs)
  ($!!) _ (Fail_Func d info) _ _ = failCons d info
  ($##) cont f@(Func _) cd cs = cont f cd cs
  ($##) cont (Choice_Func d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_Func d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_Func d c x) cd cs = guardCons d c ((cont $## x) cd $! addCs c cs)
  ($##) _ (Fail_Func d info) _ _ = failCons d info
  searchNF search cont (Func x1) = search (\y1 -> cont (Func y1)) x1
  searchNF _ _ x = internalError ("Prelude.Func.searchNF: no constructor: " ++ (show x))

instance (Unifiable t0,Unifiable t1) => Unifiable (Func t0 t1) where
  (=.=) _ _ cd _ = Fail_C_Success cd defFailInfo
  (=.<=) _ _ cd _ = Fail_C_Success cd defFailInfo
  bind _  _ (Func _) = internalError "can not bind a Func"
  bind cd i (Choice_Func d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_Func d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs 
  bind cd i (Choices_Func d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ (Choices_Func _ i@(ChoiceID _) _) = internalError ("Prelude.Func.bind: Choices with ChoiceID: " ++ (show i))
  bind _  _ (Fail_Func _ info) = [Unsolvable info]
  bind cd i (Guard_Func _ c e) = case unwrapCs c of
    Just cs -> (getConstrList cs) ++ (bind cd i e)
    Nothing -> error "Prelude.Func.bind: Called bind with a guard expression containing a non-equation constraint"
  lazyBind _  _ (Func _) = internalError "can not lazily bind a Func"
  lazyBind cd i (Choice_Func d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_Func d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_Func d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ (Choices_Func _ i _) = internalError ("Prelude.Func.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_Func _ info) = [Unsolvable info]
  lazyBind cd i (Guard_Func _ c e) = case unwrapCs c of
    Just cs -> (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind cd i e)))]  
    Nothing -> error "Prelude.Func.lazyBind: Called lazyBind with a guard expression containing a non-equation constraint"
  fromDecision _ _ _ = error "ERROR: No fromDecision for Func"   
-- END GENERATED FROM PrimTypes.curry

-- BEGIN GENERATED FROM PrimTypes.curry
data C_IO t0
     = C_IO (IO t0)
     | Choice_C_IO Cover ID (C_IO t0) (C_IO t0)
     | Choices_C_IO Cover ID ([C_IO t0])
     | Fail_C_IO Cover FailInfo
     | Guard_C_IO Cover WrappedConstraint (C_IO t0)

instance Show (C_IO a) where show = internalError "show for C_IO"

instance Read (C_IO a) where readsPrec = internalError "readsPrec for C_IO"

instance NonDet (C_IO t0) where
  choiceCons = Choice_C_IO
  choicesCons = Choices_C_IO
  failCons = Fail_C_IO
  guardCons = Guard_C_IO
  try (Choice_C_IO cd i x y) = tryChoice cd i x y
  try (Choices_C_IO cd i xs) = tryChoices cd i xs
  try (Fail_C_IO cd info) = Fail cd info
  try (Guard_C_IO cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_IO cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_IO cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_IO cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_IO _ i _) = internalError ("Prelude.IO.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_IO cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_IO cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable (C_IO a) where generate _ _ = internalError "generate for C_IO"

instance (NormalForm t0) => NormalForm (C_IO t0) where
  ($!!) cont io@(C_IO _) cd cs = cont io cd cs
  ($!!) cont (Choice_C_IO d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_C_IO d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_C_IO d c x) cd cs = guardCons d c ((cont $!! x) cd $! addCs c cs)
  ($!!) _ (Fail_C_IO d info) _ _ = failCons d info
  ($##) cont io@(C_IO _) cd cs = cont io cd cs
  ($##) cont (Choice_C_IO d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_C_IO d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_C_IO d c x) cd cs = guardCons d c ((cont $## x) cd $! addCs c cs)
  ($##) _ (Fail_C_IO d info) _ _ = failCons d info
  searchNF _ cont io@(C_IO _) = cont io
  searchNF _ _ x = internalError ("Prelude.IO.searchNF: no constructor: " ++ (show x))

instance Unifiable t0 => Unifiable (C_IO t0) where
  (=.=) _ _ cd _ = Fail_C_Success cd defFailInfo
  (=.<=) _ _ cd _ = Fail_C_Success cd defFailInfo
  bind _  _(C_IO _) = internalError "can not bind IO"
  bind cd i (Choice_C_IO d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_IO d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_IO d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ (Choices_C_IO _ i _) = internalError ("Prelude.IO.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_IO _ info) = [Unsolvable info]
  bind cd i (Guard_C_IO _ c e) = case unwrapCs c of
    Just cs -> (getConstrList cs) ++ (bind cd i e)
    Nothing -> error "Prelude.IO.bind: Called bind with a guard expression containing a non-equation constraint"
  lazyBind _  _ (C_IO _)            = internalError "can not lazily bind IO"
  lazyBind cd i (Choice_C_IO d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_IO d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_IO d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ (Choices_C_IO _ i@(ChoiceID _) _) = internalError ("Prelude.IO.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _  _ (Fail_C_IO cd info) = [Unsolvable info]
  lazyBind cd i (Guard_C_IO _ c e) = case unwrapCs c of
    Just cs -> (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind cd i e)))]
    Nothing -> error "Prelude.IO.lazyBind: Called lazyBind with a guard expression containing a non-equation constraint"
  fromDecision _ _ _ = error "ERROR: No fromDecision for C_IO"
-- END GENERATED FROM PrimTypes.curry

instance ConvertCurryHaskell ca ha => ConvertCurryHaskell (C_IO ca) (IO ha)
  where
  toCurry io  = C_IO (io >>= return . toCurry)
  fromCurry _ = internalError "C_IO.fromCurry: Use top-level search instead."

-- ---------------------------------------------------------------------------
-- Primitive data that is built-in (e.g., Handle, IORefs,...)
-- ---------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry
data PrimData t0
     = PrimData t0
     | Choice_PrimData Cover ID (PrimData t0) (PrimData t0)
     | Choices_PrimData Cover ID ([PrimData t0])
     | Fail_PrimData Cover FailInfo
     | Guard_PrimData Cover (WrappedConstraint) (PrimData t0)

instance Show (PrimData a) where show = internalError "show for PrimData"

instance Read (PrimData a) where readsPrec = internalError "readsPrec for PrimData"

instance NonDet (PrimData t0) where
  choiceCons = Choice_PrimData
  choicesCons = Choices_PrimData
  failCons = Fail_PrimData
  guardCons = Guard_PrimData
  try (Choice_PrimData cd i x y) = tryChoice cd i x y
  try (Choices_PrimData cd i xs) = tryChoices cd i xs
  try (Fail_PrimData cd info) = Fail cd info
  try (Guard_PrimData cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_PrimData cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_PrimData cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_PrimData cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_PrimData _ i@(ChoiceID _) _) = internalError ("Prelude.PrimData.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_PrimData cd info) = f cd info
  match _ _ _ _ f _ (Guard_PrimData cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable (PrimData a) where generate _ _ = internalError "generate for PrimData"

instance NormalForm (PrimData a) where
  ($!!) cont p@(PrimData _) cd cs = cont p cd cs
  ($!!) cont (Choice_PrimData d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_PrimData d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_PrimData d c x) cd cs = guardCons d c ((cont $!! x) cd $! addCs c cs)
  ($!!) _ (Fail_PrimData d info) _ _ = failCons d info
  ($##) cont p@(PrimData _) cd cs = cont p cd cs
  ($##) cont (Choice_PrimData d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_PrimData d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_PrimData d c x) cd cs = guardCons d c ((cont $## x) cd $! addCs c cs)
  ($##) _ (Fail_PrimData d info) _ _ = failCons d info
  -- no search inside argument of PrimData since it is primitive:
  searchNF _ cont (PrimData x) = cont (PrimData x)
  searchNF _ _ x = internalError ("Prelude.PrimData.searchNF: no constructor: " ++ (show x))

instance Unifiable (PrimData t0) where
  (=.=) _ _ cd _ = Fail_C_Success cd defFailInfo
  (=.<=) _ _ cd _ = Fail_C_Success cd  defFailInfo
  bind _  _ (PrimData _) = internalError "can not bind PrimData"
  bind cd i (Choice_PrimData d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_PrimData d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_PrimData d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ (Choices_PrimData _ i _) = internalError ("Prelude.PrimData.bind: Choices with ChoiceID: " ++ (show i))
  bind _  _ (Fail_PrimData _ info) = [Unsolvable info]
  bind cd i (Guard_PrimData _ c e) = case unwrapCs c of
    Just cs -> (getConstrList cs) ++ (bind cd i e)
    Nothing -> error "Prelude.PrimData.bind: Called bind with a guard expression containing a non-equation constraint"
  lazyBind _  _ (PrimData _) = internalError "can not lazily bind PrimData"
  lazyBind cd i (Choice_PrimData d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_PrimData d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_PrimData d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ (Choices_PrimData _ i@(ChoiceID _) _) = internalError ("Prelude.PrimData.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _  _ (Fail_PrimData _ info) = [Unsolvable info]
  lazyBind cd i (Guard_PrimData _ c e) = case unwrapCs c of
    Just cs -> (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind cd i e)))]
    Nothing -> error "Prelude.PrimData.lazyBind: Called lazyBind with a guard expression containing a non-equation constraint"
  fromDecision _ _ _ = error "ERROR: No fromDecision for PrimData"


-- END GENERATED FROM PrimTypes.curry

instance ConvertCurryHaskell (PrimData a) a where -- needs FlexibleInstances
  fromCurry (PrimData a) = a
  fromCurry _            = internalError "PrimData with no ground term occurred"
  toCurry a = PrimData a

-- --------------------------------------------------------------------------
-- Our own implemenation of file handles (put here since used in various
-- libraries)
-- --------------------------------------------------------------------------

-- since the operation IOExts.connectToCmd uses one handle for reading and
-- writing, we implement handles either as a single handle or two handles:
data CurryHandle = OneHandle Handle | InOutHandle Handle Handle

inputHandle :: CurryHandle -> Handle
inputHandle (OneHandle h)     = h
inputHandle (InOutHandle h _) = h

outputHandle :: CurryHandle -> Handle
outputHandle (OneHandle h)     = h
outputHandle (InOutHandle _ h) = h
