-- Benchmark to compare BFS vs. IDS

-- return nondeterministically all numbers starting from a given value
f n = f (n+1) ? n

main | f 0 == 25000 = success

g n = g (n+1) ? n ? g (n + 1)

main2 | g 0 == 29 = success

main3 | g 0 == 26 = success

main4 | g 0 == 9  = success
