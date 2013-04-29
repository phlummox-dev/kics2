---------------------------------------------------------------------
--- This module provides a transformation on FlatCurry programs.
---
--- @author Bjoern Peemoeller
--- @version April 2013
---------------------------------------------------------------------
module EnsureRigidity (ensureRigidity) where

import FlatCurry
import FlatCurryGoodies

ensureRigidity :: Prog -> Prog
ensureRigidity p = updProgFuncs (map transFunc) p

transFunc :: FuncDecl -> FuncDecl
transFunc f@(Func _  _ _ _ (External _)) = f
transFunc (Func   qn k v t (Rule  vs e)) = Func qn k v t (Rule vs e')
  where e' = transExpr e

transExpr :: Expr -> Expr
transExpr e = trExpr Var Lit Comb Let Free Or transCase Branch Typed e
 where
  -- This is where the interesting stuff happens, if a call to
  -- cond is found, the call to an auxiliary function is generated
  transCase Flex  s bs = Case Flex s bs
  transCase Rigid s bs = Case Flex (addENFCall s) bs

  addENFCall x = Comb FuncCall ("Prelude", "ensureNotFree") [x]
