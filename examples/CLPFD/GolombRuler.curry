-- A Golomb ruler may be defined as a set of m integers
-- 0 = a_1 < a_2 < ... < a_m such that the m(m−1)/2 differences
-- a_j − a_i, 1 <= i < j <= m are distinct.
-- Such a ruler is said to contain m marks and is of length am.
-- The objective is to find optimal (minimum length) or near optimal rulers.
--
-- (description from http://www.csplib.org/Problems/prob006/)

import CLPFD2

golombRuler :: Int -> [[Int]]
golombRuler n
  = let vars        = take n (domain 0 (n*n))
        dn          = (n*n-n) `div` 2
        d           = take dn (domain 0 (n*n))
        diag i j    = d !# (((i *# (fdc 2 *# fdc n -# i -# fdc 1)) /# fdc 2) +# j -# i -# fdc 1)
        constraints = vars !# fdc 0 =# fdc 0 /\
                      allDifferent d /\
                      sorted vars /\
                      loopall (fdc 1) (fdc (n-1)) (\j ->
                        diag (fdc 0) j =# vars !# j) /\
                      loopall (fdc 1) (fdc (n-2)) (\i ->
                        loopall (i +# fdc 1) (fdc (n-1)) (\j ->
                          diag i j =# (vars !# j) -# (vars !# i))) /\
                      loopall (fdc 0) (fdc (n-1)) (\i ->
                        loopall (i +# fdc 1) (fdc (n-1)) (\j ->
                          diag i j >=# (j -# i) *# (j -# i +# fdc 1) /# fdc 2)) /\
                      diag (fdc 0) (fdc 1) <=# diag (fdc (n-2)) (fdc (n-1))
    in solveFDVars [GecodeRuntime] constraints vars
