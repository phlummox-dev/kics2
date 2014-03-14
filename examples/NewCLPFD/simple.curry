import NewCLPFD

runTest = runFD test2

test1 = newVar 0 3 >>=# \x -> x `hasValue` 2 >># labeling [x]

test2 = newVar 0 3 >>=# \x -> newVar 0 3 >>=# \y -> x <# y >># x `hasValue` 2 >># labeling [x,y]

