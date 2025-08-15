module Main where

-- 1. Reversing a List with Folds

-- reverse list with foldr

reverseWithFoldr lst =
    foldr func [] lst
        where
            func listElement accumulator =
                accumulator <> [listElement]

-- reverse list with foldl

reverseWithFoldl lst =
    foldl func [] lst
        where 
            func accumulator listElement =
                listElement : accumulator

reverseWithFoldl' lst = foldl (flip (:)) [] lst


{- 
    The foldl implementation is simpler

    The foldl version is generally more efficient in both time and space.

    foldl: O(n) each prepending step with : is O(1), and we do n steps.
    foldr: O(n^2) the ++ operator copies its left argument each time. 
            As the accumulator grows, each append traverses the entire current list, 
            leading to quadratic time 
            (e.g., for a list of length n, the total work is roughly 1 + 2 + ... + n = n(n+1)/2).    
-}

-- 2. Implement the zipWith function with and without using list comprehensions. 
--    Can you implement zipWith using foldl?

-- zipWith with manual recursion

zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs
zipWith' _f _as _bs = []

-- zipWith' (,) [1,2,3,4] ["A", "B", "C"]
-- [(1,"A"),(2,"B"),(3,"C")]

-- zipWithFoldl using zip

zipWithFoldl f as bs = reverse $ foldl func [] zipped
    where
        zipped = zip as bs
        func accumulator (a, b) = f a b : accumulator

-- zipWithFoldl (,) [1,2,3] ["A", "B", "C", "D"]
--  [(1,"A"),(2,"B"),(3,"C")]

-- zipWithFoldr using zip

zipWithFoldr f as bs = foldr func [] zipped
    where 
        zipped = zip as bs
        func (a, b) accumulator = f a b : accumulator

--  zipWithFoldr (,) [1,2,3] ["A", "B", "C", "D"]
-- [(1,"A"),(2,"B"),(3,"C")]
    

main = putStrLn $ show $ "Hello, World!"