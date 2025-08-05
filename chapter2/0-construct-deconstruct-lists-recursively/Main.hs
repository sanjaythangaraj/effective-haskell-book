module Main where
import Prelude hiding (foldl, foldr)

-- contructing lists

countdown n =
  if n <= 0
    then []
    else n : countdown (n - 1)

factors num =
  factors' num 2
  where
    factors' num fact
      | num == 1 = []
      | (num `rem` fact) == 0 = fact : factors' (num `div` fact) fact
      | otherwise = factors' num (fact + 1)

-- deconstructing lists

-- check if parentheses in string are balanced 
-- (that there are same number of opening and closing parantheses)

isBalanced s =
  0 == isBalanced' 0 s
  where
    isBalanced' count s
      | null s = count
      | head s == '(' = isBalanced' (count + 1) (tail s)
      | head s == ')' = isBalanced' (count - 1) (tail s)
      | otherwise = isBalanced' count (tail s)

-- generic reduce function

reduce func carryValue lst =
    if null lst then carryValue
    else
        let intermediateValue = func carryValue (head lst)
        in reduce func intermediateValue (tail lst)


isBalanced' str = 0 == reduce checkBalance 0 str
    where
    checkBalance count letter
        | letter == '(' = count + 1
        | letter == ')' = count - 1
        | otherwise = count


-- foldl, foldr

foldl func carryValue lst =
    if null lst
    then carryValue
    else foldl func (func carryValue (head lst)) (tail lst)

{-
  foldl (/) 1 [1,2,3,4,5]

  = foldl func ((/) 1 1) [2, 3, 4, 5]
  = foldl func ((/) 1 2) [3, 4,  5]
  = foldl func ((/) 0.5 3) [4, 5]
  = foldl func ((/) 0.16667 4) [5]
  = (/) 0.04166 5
  = 0.00833
-}


foldr func carryValue lst =
    if null lst
    then carryValue
    else func (head lst) $ foldr func carryValue (tail lst)


{-
    foldr (/) 1 [1,2,3,4,5]

    = (/) 1 foldr func 1 [2, 3, 4, 5] 
    = (/) 1 ( (/) 2 foldr func [3, 4, 5])
    = (/) 1 ( (/) 2 ( (/) 3 foldr func [4, 5]))
    = (/) 1 ( (/) 2 ( (/) 3 ( (/) 4 foldr func [5])))
    = (/) 1 ( (/) 2 ( (/) 3 ( (/) 4 ( (/) 5 1))))
    
    = 1 / (2 / (3 / (4 / (5 / 1))))
    = 1 / (2 / (3 / (4 / 5)))
    = 1 / (2 / (3 / 0.8))
    = 1 / (2 / 3.75)
    = 1 / 0.533333...
    ≈ 1.875
-}    


main = print "Hello World"