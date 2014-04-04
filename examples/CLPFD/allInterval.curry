import CLPFD

allInterval n = let l,d free in
  (l =:= genVars n &
  d =:= genVars (n-1) &
  domain l 0 (n-1) &
  allDifferent l &
  domain d 1 (n-1) &
  allDifferent d &
  diffList d l &
  labeling l) &> l

diffList []     _         = success
diffList (d:ds) (x:y:xys) = d =# ((y -# x) ? (x -# y)) & diffList ds (y:xys)
