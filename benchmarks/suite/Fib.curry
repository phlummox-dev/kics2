module Fib where

fib :: Int -> Int
fib n | n == 0    = 0
      | n == 1    = 1
      | otherwise = fib (n-1) + fib (n-2)

mexp :: Int
mexp = fib 39 ? fib 39