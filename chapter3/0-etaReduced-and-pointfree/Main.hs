module Main where

-- pointful - calculates the sum of a list of numbers and then
-- multiplies that sum by some constant

pointful :: [Int] -> Int -> Int
pointful xs n = foldr (+) 0 xs * n

-- etaReduced - takes a single argument xs, the list.
-- we're partially applying the result of our sum to (*)

etaReduced :: [Int] -> Int -> Int
etaReduced xs = (*) (foldr (+) 0 xs)

-- pointfree 
pointfree :: [Int] -> Int -> Int
pointfree = (*) . foldr (+) 0

main = putStrLn $ show $ "Hello World!"