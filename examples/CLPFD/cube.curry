import CLPFD

cube = let l,s,a,b,c,d,e,f,g,h free in
  (l =:= [a,b,c,d,e,f,g,h] &
  domain l 1 8 &
  allDifferent l &
  sum [a,b,c,d] =# s &
  sum [b,c,f,g] =# s &
  sum [a,b,e,f] =# s &
  sum [e,f,g,h] =# s &
  sum [a,d,e,h] =# s &
  sum [c,d,g,h] =# s &
  labeling (s:l))
  &> (s:l)
