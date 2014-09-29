-----------------------------------------------------------------------------
--- Solving Sudoku puzzles in Curry with FD constraints
-----------------------------------------------------------------------------

import CLPFD2
import List (transpose)
import Maybe (catMaybes)

-- Solving a Su Doku puzzle represented as a matrix of numbers (possibly free
-- variables):
-- usage: sudoku s1 l where l free
sudoku :: [String] -> [[Int]]
sudoku m =
  let rows        = genRows 9 9
      constraints = foldr1 (/\) (initRows rows m) /\
                    foldr1 (/\) (map allDifferent rows) /\
                    foldr1 (/\) (map allDifferent (transpose rows)) /\
                    foldr1 (/\) (map allDifferent (squaresOfNine rows))
  in solveFDVars [] constraints (concat rows)

-- translate a matrix into a list of small 3x3 squares
squaresOfNine :: [[a]] -> [[a]]
squaresOfNine [] = []
squaresOfNine (l1:l2:l3:ls) = group3Rows [l1,l2,l3] ++ squaresOfNine ls

group3Rows :: [[a]] -> [[a]]
group3Rows l123 = if null (head l123) then [] else
 concatMap (take 3) l123 : group3Rows (map (drop 3) l123)

genRows :: Int -> Int -> [[FDExpr]]
genRows size rowNr
  | rowNr == 0 = []
  | otherwise  = take size (domain 1 size) : genRows size (rowNr-1)

initField :: FDExpr -> Char -> Maybe FDConstr
initField x c | c == ' '  = Nothing
              | otherwise = let val = ord c - ord '0'
                            in Just (x =# fdc val)

initRows :: [[FDExpr]] -> [String] -> [FDConstr]
initRows rows strings = catMaybes $ concat $ zipWith (zipWith initField) rows strings

s1 :: [String]
s1 = ["9  2  5  ",
      " 4  6  3 ",
      "  3     6",
      "   9  2  ",
      "    5  8 ",
      "  7  4  3",
      "7     1  ",
      " 5  2  4 ",
      "  1  6  9"]

s2 :: [String]
s2 = ["819  5   ",
      "  2   75 ",
      " 371 4 6 ",
      "4  59 1  ",
      "7  3 8  2",
      "  3 62  7",
      " 5 7 921 ",
      " 64   9  ",
      "   2  438"]

s3 :: [String]
s3 = ["        2",
      "4   3   1",
      "    1 95 ",
      "5 28  1  ",
      "   7 2   ",
      "  7  92 4",
      " 4  2    ",
      "1 9 7   6",
      "3  5     "]