import CLPFD
import List (transpose)

magicSquare n = let ms,mss,s free in let size = n*n in
  (ms =:= (genVars size) &
  mss =:= splitInNth n ms &
  domain ms 1 size &
  allDifferent ms &
  sumAll mss s &
  sumAll (transpose mss) s &
  sumAll (diags mss) s &
  labeling ms) &> mss


splitInNth n xs = splitInNth' n n xs
 where
  splitInNth' c n xs
    | c == 0 || null xs = []
    | otherwise         = take n xs : splitInNth' (c-1) n (drop n xs)

sumAll :: [[Int]] -> Int -> Success
sumAll mss s = foldr1 (&) (map (\row -> sum row =# s) mss)

diags :: [[Int]] -> [[Int]]
diags mss = diag' mss 0 : diag' (map reverse mss) 0 : []
 where
  diag' []         _ = []
  diag' (row:rows) i = row !! i : diag' rows (i+1)
