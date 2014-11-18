--
--  1. There are five houses.
--  2. The Englishman lives in the red house.
--  3. The Spaniard owns the dog.
--  4. Coffee is drunk in the green house.
--  5. The Ukrainian drinks tea.
--  6. The green house is immediately to the right of the ivory house.
--  7. The Old Gold smoker owns snails.
--  8. Kools are smoked in the yellow house.
--  9. Milk is drunk in the middle house.
-- 10. The Norwegian lives in the first house.
-- 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
-- 12. Kools are smoked in the house next to the house where the horse is kept.
-- 13. The Lucky Strike smoker drinks orange juice.
-- 14. The Japanese smokes Parliaments.
-- 15. The Norwegian lives next to the blue house.
--
-- Now, who drinks water? Who owns the zebra?

-- colors: yellow (c1), blue (c2), red (c3), ivory (c4), green (c5)
-- nationalities: Norwegian (n1), Ukrainian (n2), Englishman (n3), Spaniard (n4), Japanese (n5)
-- drinks: orange juice (d1), milk (d2), tea (d3), water (d4), coffee (d5)
-- smokes: Chesterfields (s1), Old Gog (s2), Parliaments (s3), Lucky Strike (s4), Kools (s5)
-- pets: horse (p1), fox (p2), snails (p3), zebra (p4), dog (p5)

import CLPFD2

zebra :: [[Int]]
zebra =
  let colors@[c1,c2,c3,c4,c5] = take 5 (domain 1 5)
      nationalities@[n1,n2,n3,n4,n5] = take 5 (domain 1 5)
      drinks@[d1,d2,d3,_,d5] = take 5 (domain 1 5)
      smokes@[s1,s2,s3,s4,s5] = take 5 (domain 1 5)
      pets@[p1,p2,p3,_,p5] = take 5 (domain 1 5)
      constraints = allDifferent colors        /\
                    allDifferent nationalities /\
                    allDifferent drinks        /\
                    allDifferent smokes        /\
                    allDifferent pets          /\
                    n3 =# c3                   /\ -- rule  2
                    n4 =# p5                   /\ -- rule  3
                    d5 =# c5                   /\ -- rule  4
                    n2 =# d3                   /\ -- rule  5
                    c4 +# fdc 1 =# c5          /\ -- rule  6
                    s2 =# p3                   /\ -- rule  7
                    s5 =# c1                   /\ -- rule  8
                    d2 =# fdc 3                /\ -- rule  9
                    n1 =# fdc 1                /\ -- rule 10
                    nextTo s1 p2               /\ -- rule 11
                    nextTo s5 p1               /\ -- rule 12
                    s4 =# d1                   /\ -- rule 13
                    n5 =# s3                   /\ -- rule 14
                    nextTo n1 c2
  in solveFDVars [GecodeRuntime, AllSolutions] constraints
       (colors ++ nationalities ++ drinks ++ smokes ++ pets)

nextTo :: FDExpr -> FDExpr -> FDConstr
nextTo h1 h2 = (h1 =# h2 +# fdc 1) \/ (h1 =# h2 -# fdc 1)