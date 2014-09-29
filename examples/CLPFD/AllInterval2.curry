-- An all-interval series of length n is a sequence (x_0, ... , x_n-1)
-- where each x_i is an integer between 0 and n-1 such that the following
-- conditions hold:
--
-- * the x_i are a permutation of {0, 1, ..., n-1}
-- * the differences between adjacent values {d_1, d_2, ..., d_n-1} with
--   d_i = abs(x_i - x_i-1) form a permutation of {1, 2, ..., n-1}
--
-- (description from http://www.gecode.org/doc-latest/reference/classAllInterval.html)

import CLPFD2

allInterval :: Int -> [[Int]]
allInterval n
  | n <= 2    = []
  | otherwise =
  let vars        = take n     (domain 0 (n-1))
      diffs       = take (n-1) (domain 1 (n-1))
      v0          = fdc 0
      v1          = fdc 1
      vnm2        = fdc (n-2)
      constraints = allDifferent vars /\
                    allDifferent diffs /\
--                    loopall v0 vnm2 (\i -> ((diffs !# i) =# ((vars !# i) -# (vars !# (i +# v1)))) \/ ((diffs !# i) =# ((vars !# (i +# v1)) -# (vars !# i)))) /\
                    loopall v0 vnm2 (\i -> (diffs !# i) =# (abs ((vars !# i) -# (vars !# (i +# v1))))) /\
                    vars !# v0 <# vars !# v1 /\
                    diffs !# v0 ># diffs !# vnm2
  in solveFDVars [GecodeSearch] constraints vars
