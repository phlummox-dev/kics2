-------------------------------------------------------------------
--- Executable specification of sequence comparison
---
--- @author Michael Hanus, Robert Giegerich, Bastian Holst
--- @version March 2014
-------------------------------------------------------------------

import ParallelSearch
import SetFunctions
import SearchTree
import RedBlackTree
import List
import Integer (minlist)

--------------------------------------------------------------------------------
------------------------------Basic definitions---------------------------------
--------------------------------------------------------------------------------

--- Representation of edit operations to align strings
data Edit = Rep Letter Letter Edit -- replace a letter with the same or another letter
          | Del Letter Edit        -- delete  a letter
          | Ins Letter Edit        -- insert  a letter
          | Mty                    -- the end

-- Algebra UnitDistance
value :: Edit -> Int
value (Rep a b x) = value x + if a==b then 0 else 1
value (Ins _ x) = 1 + value x
value (Del _ x) = 1 + value x
value Mty = 0

--------------------------------------------------------------------------------
-----------------------------Example instances----------------------------------
--------------------------------------------------------------------------------

-- The letters in the string
data Letter = A | C | G | T | I | R |N | E | D | L

-- Example strings
airline = [A,I,R,L,I,N,E]
darling = [D,A,R,L,I,N,G]

air = [A,I,R]
dar = [D,A,R]

--------------------------------------------------------------------------------
----------------------Initial definition----------------------------------------
--------------------------------------------------------------------------------

-- Generator for tree of edit operations up to a given maximal length.
editMax m =
  if m==0
  then Mty
  else Rep _ _ (editMax (m-2)) ? Del _ (editMax (m-1))
       ? Ins _ (editMax (m-1)) ? Mty

editMax' m =
  if m<=0
  then Mty
  else Rep _ _ (editMax' (m-2)) ? Del _ (editMax' (m-1))
       ? Ins _ (editMax' (m-1)) ? Mty

-- Rewrite system for left interpretation of edits to strings
simpL :: Edit -> [Letter]
simpL (Rep a _ x) = a : simpL x
simpL (Del a x) = a : simpL x
simpL (Ins _ x) = simpL x
simpL Mty = []

-- Rewrite system for right interpretation of edits to strings
simpR :: Edit -> [Letter]
simpR (Rep _ b x) = b : simpR x
simpR (Del _ x) = simpR x
simpR (Ins a x) = a : simpR x
simpR Mty = []

-- Rewrite system for left AND right interpretation of edits to strings
simpLR :: Edit -> ([Letter],[Letter])
simpLR (Rep a b x) = (a : x1, b : x2)  where (x1,x2) = simpLR x
simpLR (Del a x) = (a : x1, x2) where (x1,x2) = simpLR x
simpLR (Ins a x) = (x1, a : x2)  where (x1,x2) = simpLR x
simpLR Mty = ([],[])

--------------------------------------------------------------------------------

-- Computing inverse of left/right rewrite system:
main1 | simpL e =:= airline & simpR e =:= darling
      = (value e, e)
 where e free

main2 | simpL e =:= air & simpR e =:= dar
     = (value e, e)
 where e free

-- Computing inverse of left/right rewrite system:
main3 | simpLR e =:= (airline,darling) --(air,dar)
     = (value e, e)
 where e = editMax' 14

-- Computing inverse of left/right rewrite system:
-- with minimal distance:
main strategy =
  let s = set0With strategy main3
      (m,_) = minValue (\x y -> fst x <= fst y) s
  in do putStrLn $ "Minimum: " ++ show m
        vlist <- values2list s
        putStrLn (unlines (map show (filter (\x -> fst x == m) vlist)))

main_par strategy = do
  vlist <- getLazyValues strategy main3
  let m = minlist $ map fst vlist
  putStrLn $ "Minimum: " ++ show m
  putStrLn (unlines (map show (filter (\x -> fst x == m) vlist)))

--------------------------------------------------------------------------------
----------------------Simple definition ----------------------------------------
--------------------------------------------------------------------------------

edit :: [Letter] -> [Letter] -> Edit
edit []     []     = Mty
edit ls     (r:rs) = Ins r (edit ls rs)
edit (l:ls) rs     = Del l (edit ls rs)
edit (l:ls) (r:rs) = Rep l r (edit ls rs)

--------------------------------------------------------------------------------

main3_simple =
  let e = edit airline darling
  in (value e, e)

main_simple strategy =
  let s = set0With strategy main3_simple
      (m,_) = minValue (\x y -> fst x <= fst y) s
  in do putStrLn $ "Minimum: " ++ show m
        vlist <- values2list s
        putStrLn (unlines (map show (filter (\x -> fst x == m) vlist)))

main_simple_par strategy = do
  vlist <- getLazyValues strategy main3_simple
  let m = minlist $ map fst vlist
  putStrLn $ "Minimum: " ++ show m
  putStrLn (unlines (map show (filter (\x -> fst x == m) vlist)))
