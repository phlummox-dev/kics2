-- A magic sequence of length n is a sequence of integers x_0, ..., x_n−1
-- between 0 and n−1, such that for all i in 0 to n−1,
-- the number i occurs exactly x_i times in the sequence.
-- For example for n=4 [1,2,1,0] is a magic sequence, since 0 occurs once,
-- 1 occurs twice etc.

import CLPFD2

magicSeries :: Int -> [[Int]]
magicSeries n =
  let series         = take n (domain 0 (n-1))
      count list val = sum $ map (\v -> channel (v =# val)) list
      constraints    = loopall (fdc 0) (fdc (n-1)) (\i ->
                         count series i =# series !# i) /\
                       sum series =# fdc n
  in solveFDVars [GecodeSearch, AllSolutions] constraints series