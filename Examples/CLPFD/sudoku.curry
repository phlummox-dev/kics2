-----------------------------------------------------------------------------
--- Solving Sudoku puzzles in Curry with FD constraints
---
--- @author Michael Hanus
--- @version December 2005
--- modified by Jan Tikovsky July 2012
-----------------------------------------------------------------------------

import CLPFD
import List

-- Solving a Su Doku puzzle represented as a matrix of numbers (possibly free
-- variables):
sudoku :: [String] -> [[Int]] -> Success
sudoku m l =
 l =:= readSudoku m &
 domain (concat l) 1 9 &                         -- define domain of all digits
 foldr1 (&) (map allDifferent l)  &             -- all rows contain different digits
 foldr1 (&) (map allDifferent (transpose l))  & -- all columns have different digits
 foldr1 (&) (map allDifferent (squaresOfNine l)) & -- all 3x3 squares are different
 labeling (concat l)

-- translate a matrix into a list of small 3x3 squares
squaresOfNine :: [[a]] -> [[a]]
squaresOfNine [] = []
squaresOfNine (l1:l2:l3:ls) = group3Rows [l1,l2,l3] ++ squaresOfNine ls

group3Rows l123 = if null (head l123) then [] else
 concatMap (take 3) l123 : group3Rows (map (drop 3) l123)

-- read a Su Doku specification written as a list of strings containing digits
-- and spaces
readSudoku :: [String] -> [[Int]]
readSudoku s = map (map transDigit) s
 where
   transDigit c = if c==' ' then x else ord c - ord '0'
      where x free

-- usage: sudoku s1 l where l free

s1 = ["9  2  5  ",
      " 4  6  3 ",
      "  3     6",
      "   9  2  ",
      "    5  8 ",
      "  7  4  3",
      "7     1  ",
      " 5  2  4 ",
      "  1  6  9"]

s2 = ["819  5   ",
      "  2   75 ",
      " 371 4 6 ",
      "4  59 1  ",
      "7  3 8  2",
      "  3 62  7",
      " 5 7 921 ",
      " 64   9  ",
      "   2  438"]
