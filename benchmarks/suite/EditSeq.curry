-------------------------------------------------------------------
--- Executable specification of sequence comparison
---
--- @author Michael Hanus, Robert Giegerich
--- @version January 2013
-------------------------------------------------------------------

import ParallelSearch
import SetFunctions
import RedBlackTree
import List

--- Representation of edit operations to align strings
data Edit = Rep Letter Letter Edit
          | Del Letter Edit
          | Ins Letter Edit
          | Mty

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

-- The letters in the string
data Letter = A | C | G | T | I | R |N | E | D | L

-- Algebra UnitDistance
value :: Edit -> Int
value (Rep a b x) = value x + if a==b then 0 else 1
value (Ins _ x) = 1 + value x
value (Del _ x) = 1 + value x
value Mty = 0

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

-- Example strings
airline = [A,I,R,L,I,N,E]
darling = [D,A,R,L,I,N,G]

air = [A,I,R]
dar = [D,A,R]

-- Computing inverse of left/right rewrite system:
main1 | simpL e =:= airline & simpR e =:= darling
      = (value e, e)
 where e free

main2 | simpL e =:= air & simpR e =:= dar
     = (value e, e)
 where e free


-- Rewrite system for left AND right interpretation of edits to strings
simpLR :: Edit -> ([Letter],[Letter])
simpLR (Rep a b x) = (a : x1, b : x2)  where (x1,x2) = simpLR x
simpLR (Del a x) = (a : x1, x2) where (x1,x2) = simpLR x
simpLR (Ins a x) = (x1, a : x2)  where (x1,x2) = simpLR x
simpLR Mty = ([],[])

-- Computing inverse of left/right rewrite system:
main3 | simpLR e =:= (airline,darling) --(air,dar)
     = (value e, e)
 where e = editMax' 14

-- Computing inverse of left/right rewrite system:
-- with minimal distance:
main4 = let s = set0 main3
            (m,_) = minValue (\x y -> fst x <= fst y) s
         in do putStrLn $ "Minimum: " ++ show m
               vlist <- values2list s
               putStrLn (unlines (map show (filter (\x -> fst x == m) vlist)))
-->Minimum: 3
-->(3,(Rep A D (Rep I A (Rep R R (Rep L L (Rep I I (Rep N N (Rep E G Mty))))))))
-->(3,(Ins D (Rep A A (Del I (Rep R R (Rep L L (Rep I I (Rep N N (Rep E G Mty)))))))))

main_par_lazy = do
  vlist <- getLazyValues splitAll main3
  let ms@(m:_) = sortBy (\x y -> fst x <= fst y) vlist
  putStrLn $ "Minimum: " ++ show (fst m)
  putStrLn (unlines (map show (takeWhile (\x -> (fst x) == (fst m)) ms)))

main_par_strict = do
  vlist <- getAllValues splitAll main3
  let ms@(m:_) = sortBy (\x y -> fst x <= fst y) vlist
  putStrLn $ "Minimum: " ++ show (fst m)
  putStrLn (unlines (map show (takeWhile (\x -> (fst x) == (fst m)) ms)))

{-
Benchmarks on lafite (Intel i5, 1.2GHz) with KiCS2 in seconds:
main4: 18.77

Benchmarking search strategies with goal "fst main3 =:= 3":
dfs: 18.04
bfs: 26.33
par 1: 23.77
par 2: 12.74
par 3: 11.11
par 4: 11.71
par  : 10.67
-}
