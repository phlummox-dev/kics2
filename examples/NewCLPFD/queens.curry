import NewCLPFD

queens n = runFD $ newVars n 1 n
   >>=# \qs -> allSafe qs
   >>#  labeling qs

allSafe []     = osuccess
allSafe (q:qs) = (cval 1)
   >>=# \p -> safe q qs p
   >>#  allSafe qs

safe _ []      _ = osuccess
safe q (q1:qs) p = no_attack q q1 p
   >>#  inc p
   >>=# \p' -> safe q qs p'

no_attack q1 q2 p = q1 /=# q2
   >>#  q2+#p
   >>=# \q2pp -> q1 /=# q2pp
   >>#  q2-#p
   >>=# \q2mp -> q1 /=# q2mp

inc p = cval 1 >>=# \one -> p+#one
