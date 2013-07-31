import CLPFD

-- solving the n-queens problem in Curry with FD constraints:

queens n = let l free in
  ( genVars n =:= l &
    domain l 1 (length l) &
    all_safe l &
    labelingWith FirstFail l
  ) &> l

all_safe [] = success
all_safe (q:qs) = safe q qs 1 & all_safe qs

safe :: Int -> [Int] -> Int -> Success
safe _ [] _ = success
safe q (q1:qs) p = no_attack q q1 p & safe q qs (p+#1)

no_attack q1 q2 p = q1 /=# q2 & q1 /=# q2+#p & q1 /=# q2-#p
