import CLPFD2

queens :: Int -> [[Int]]
queens n =
  let qs          = take n (domain 1 n)
      offsets     = map fdc [1..]
      constraints = allDifferent qs /\
                    allDifferent (zipWith (+#) qs offsets) /\
                    allDifferent (zipWith (-#) qs offsets)
  in solveFD [GecodeSearch,MiddleOut,AllSolutions] constraints

queensHO :: Int -> [[Int]]
queensHO n =
  let qs          = take n (domain 1 n)
      constraints = loopall (fdc 0) (fdc (n-2)) $ \i ->
                      loopall (i +# (fdc 1)) (fdc (n-1)) $ \j ->
                        noattack i j (qs !# i) (qs !# j)
  in solveFD [GecodeSearch] constraints

noattack :: FDExpr -> FDExpr -> FDExpr -> FDExpr -> FDConstr
noattack i j qi qj =
  qi      /=# qj      /\
  qi +# i /=# qj +# j /\
  qi -# i /=# qj -# j