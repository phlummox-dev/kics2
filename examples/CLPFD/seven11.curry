-- seven eleven problem 
-- (description copied from the grocery example of the monadic constraint programming framework
-- by Tom Schrijvers, Peter Stuckley and Philip Wadler) 
-- A kid goes into a grocery store and buys four items. The cashier charges $7.11. 
-- The kid pays and is about to leave when the cashier calls the kid back, and says 
-- "Hold on, I multiplied the four items instead of adding them; I'll try again... 
-- Gosh, with adding them the price still comes to $7.11"! What were the prices of 
-- the four items?

import CLPFD

seven11 :: [Int]
seven11 = let l,a,b,c,d free in
  ( l =:= [a,b,c,d] &
    domain l 1 711 &
    a <=# b & b <=# c & c <=# d &
    sum [a,b,c,d] =# 711 &
    (a *# b) *# (c *# d) =# 711000000 &
    labeling l
  ) &> l
