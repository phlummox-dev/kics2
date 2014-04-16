import List

perm xs =
  perm' xs []
 where
  perm' xs ys =
    case (xs, ys) of
      ([],  []) -> []
      ([],  _ ) -> perm' ys []
      ([x], _)  -> x : perm' ys []
      _         ->
        let xl = length xs
            (xs1, xs2) = splitAt (xl `div` 2) xs
        in perm' xs1 (xs2 ++ ys) ? perm' xs2 (xs1 ++ ys)


sorted :: [Int] -> [Int]
sorted []       = []
sorted [x]      = [x]
sorted (x:y:ys) | x <= y = x : sorted (y:ys)

psort xs = sorted (perm xs)

sortmain n = psort (2:[n,n-1 .. 3]++[1])

main = sortmain 15

main2 = sortmain 12

main3 = psort [9,8,7,6,5,4,3,2,1]

main4 = psort $ take 16 (repeat 1)

main5 = psort $ [1,9,5,10,3,11,6,12,2,13,7,14,4,14,8,16]

main6 = psort $ take 12 (cycle [1,2])