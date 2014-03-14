import NewCLPFD

cube = runFD $ newVars 8 1 8
   >>=# \l@[a,b,c,d,e,f,g,h] -> allDifferent l
   >>#  newVar 4 32
   >>=# \s -> sum [a,b,c,d] >>=# \s1 -> s1 =# s
   >>#        sum [b,c,f,g] >>=# \s2 -> s2 =# s
   >>#        sum [a,b,e,f] >>=# \s3 -> s3 =# s
   >>#        sum [e,f,g,h] >>=# \s4 -> s4 =# s
   >>#        sum [a,d,e,h] >>=# \s5 -> s5 =# s
   >>#        sum [c,d,g,h] >>=# \s6 -> s6 =# s
   >>#  labeling (s:l)

