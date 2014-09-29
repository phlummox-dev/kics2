-- seven eleven problem
-- (description copied from the grocery example of the monadic constraint
-- programming framework by Tom Schrijvers, Peter Stuckley and Philip Wadler)
-- A kid goes into a grocery store and buys four items. The cashier charges
-- $7.11.
-- The kid pays and is about to leave when the cashier calls the kid back,
-- and says "Hold on, I multiplied the four items instead of adding them;
-- I'll try again...
-- Gosh, with adding them the price still comes to $7.11"! What were the prices
-- of the four items?

import CLPFD2

seven11 = let l@[a,b,c,d] = take 4 (domain 0 711)
              constraints = allDifferent l /\
                            sorted l /\ --a <=# b /\ b <=# c /\ c <=# d /\
                            a +# b +# c +# d =# fdc 711 /\
                            (a *# b) *# (c *# d) =# fdc 711000000
          in solveFD [GecodeRuntime,AllSolutions] constraints