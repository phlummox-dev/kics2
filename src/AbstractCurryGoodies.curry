--- --------------------------------------------------------------------------
--- Auxiliaries for processing AbstractCurry type expressions.
---
--- @author Michael Hanus
--- @version May 2014
--- --------------------------------------------------------------------------
module AbstractCurryGoodies
  ( isPolyType, isFunctionalType, isIOType, isIOReturnType, modsOfType
  , showMonoTypeExpr
  ) where

import AbstractCurry
import List          (intercalate, union)

import Utils         (notNull)

--- Returns true if the type expression contains type variables.
isPolyType :: CTypeExpr -> Bool
isPolyType (CTVar                _) = True
isPolyType (CFuncType domain range) = isPolyType domain || isPolyType range
isPolyType (CTCons      _ typelist) = any isPolyType typelist
isPolyType (CRecordType fields   _) = any isPolyType (map snd fields)

--- Returns true if the type expression is a functional type.
isFunctionalType :: CTypeExpr -> Bool
isFunctionalType texp = case texp of
  CFuncType _ _ -> True
  _             -> False

--- Returns true if the type expression is (IO t).
isIOType :: CTypeExpr -> Bool
isIOType texp = case texp of
  CTCons tc _ -> tc == (prelude, "IO")
  _           -> False

--- Returns true if the type expression is (IO t) with t/=() and
--- t is not functional
isIOReturnType :: CTypeExpr -> Bool
isIOReturnType (CTVar            _) = False
isIOReturnType (CFuncType      _ _) = False
isIOReturnType (CTCons tc typelist) =
  tc==(prelude,"IO") && head typelist /= CTCons (prelude,"()") []
  && not (isFunctionalType (head typelist))
isIOReturnType (CRecordType    _ _) = False

--- Returns all modules used in the given type.
modsOfType :: CTypeExpr -> [String]
modsOfType (CTVar            _) = []
modsOfType (CFuncType    t1 t2) = modsOfType t1 `union` modsOfType t2
modsOfType (CTCons (mod,_) tys) = foldr union [mod] $ map modsOfType tys
modsOfType (CRecordType flds _) = foldr union [] $ map (modsOfType . snd) flds

showMonoTypeExpr :: Bool -> CTypeExpr -> String
showMonoTypeExpr mono ty = showMonoTypeExpr' mono False ty

--- Shows an AbstractCurry type expression in standard Curry syntax.
--- If the first argument is True, all occurrences of type variables
--- are replaced by "()".
--- If the second argument is True, the type expression is enclosed
--- in brackets.
showMonoTypeExpr' :: Bool -> Bool -> CTypeExpr -> String
showMonoTypeExpr' mono _      (CTVar             (_,name)) =
  if mono then "()" else showIdentifier name
showMonoTypeExpr' mono nested (CFuncType     domain range) = parens nested $
  showMonoTypeExpr' mono (isFunctionalType domain) domain
  ++ " -> " ++ showMonoTypeExpr' mono False range
showMonoTypeExpr' mono nested (CTCons (mod,name) typelist)
  | mod==prelude && name == "untyped"
  = "-"
  | otherwise
  = parens (nested && notNull typelist) (showTypeCons mono mod name typelist)
showMonoTypeExpr' mono nested (CRecordType       fields _) =
  '{' : intercalate ", " (map (showField mono nested) fields) ++ "}"

showTypeCons :: Bool -> String -> String -> [CTypeExpr] -> String
showTypeCons _    _   name []       = name
showTypeCons mono mod name ts@(_:_)
  | mod == prelude = showPreludeTypeCons mono name ts
  | otherwise      = name ++ prefixMap (showMonoTypeExpr' mono True) ts " "

showPreludeTypeCons :: Bool -> String -> [CTypeExpr] -> String
showPreludeTypeCons mono name typelist
  | name == "[]" && head typelist == CTCons (prelude,"Char") []
  = "String"
  | name == "[]"
  = "[" ++ showMonoTypeExpr' mono False (head typelist) ++ "]"
  | isTuple name
  = "(" ++ combineMap (showMonoTypeExpr' mono False) typelist "," ++ ")"
  | otherwise
  = name ++ prefixMap (showMonoTypeExpr' mono True) typelist " "

showField :: Bool -> Bool -> CField CTypeExpr -> String
showField mono nested (lbl, ty)
  = lbl ++ " :: " ++ showMonoTypeExpr' mono nested ty

-- Remove characters '<' and '>' from identifiers since these characters
-- are sometimes introduced in new identifiers generated by the front end
-- (for sections)
showIdentifier :: String -> String
showIdentifier = filter (`notElem` "<>")

prelude :: String
prelude = "Prelude"

-- enclose string with parentheses if required by first argument
parens :: Bool -> String -> String
parens True  s = '(' : s ++ ")"
parens False s = s

prefixMap :: (a -> [b]) -> [a] -> [b] -> [b]
prefixMap f xs s = concatMap (s ++) (map f xs)

combineMap :: (a -> [b]) -> [a] -> [b] -> [b]
combineMap _ []     _ = []
combineMap f (x:xs) s = f x ++ prefixMap f xs s

isTuple :: String -> Bool
isTuple []     = False
isTuple (x:xs) = x == '(' && p1_isTuple xs
  where
  p1_isTuple []         = False
  p1_isTuple (z:[])     = z == ')'
  p1_isTuple (z1:z2:zs) = z1 == ',' && p1_isTuple (z2:zs)
