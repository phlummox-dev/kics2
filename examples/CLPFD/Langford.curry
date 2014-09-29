-- Goal: Arrange k sets of numbers 1 to n,
-- so that each appearance of the number m is m numbers on from the last

import CLPFD2

langford :: Int -> Int -> [[Int]]
langford k n =
  let sequence    = take (k*n) (domain 1 n)
      pos         = take (k*n) (domain 0 (k*n-1))
      constraints = allDifferent pos /\
                    loopall (fdc 0) (fdc (n-1)) (\i ->
                      loopall (fdc 0) (fdc (k-2)) (\j ->
                        pos !# (i *# fdc k +# j) +# i +# fdc 2 =# pos !# (i *# fdc k +# j +# fdc 1))) /\
                    loopall (fdc 0) (fdc (n-1)) (\i ->
                      loopall (fdc 0) (fdc (k-1)) (\j ->
                        sequence !# (pos !# (i *# fdc k +# j)) =# i +# fdc 1)) /\
                    sequence !# fdc 0 <# sequence !# (fdc n *# fdc k -# fdc 1)
  in solveFDVars [GecodeSearch] constraints sequence