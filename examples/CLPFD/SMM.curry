import CLPFD2

smm :: [[Int]]
smm =
 let xs@[s,e,n,d,m,o,r,y] = take 8 (domain 0 9)
     constraints =
        s ># fdc 0 /\
        m ># fdc 0 /\
        allDifferent xs /\
           fdc 1000  *# s +# fdc 100  *# e +# fdc 10  *# n +# d
        +# fdc 1000  *# m +# fdc 100  *# o +# fdc 10  *# r +# e
        =# fdc 10000 *# m +# fdc 1000 *# o +# fdc 100 *# n +# fdc 10 *# e +# y
 in solveFDVars [] constraints xs