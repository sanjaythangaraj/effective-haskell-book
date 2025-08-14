module Main where

-- a stream or a generator
-- Unlike recursive functions, streams don't count down to a base case.
-- Instead, they start with a seed value and work their way up, potentially forever.

numbersStartingAt n =
  n : numbersStartingAt (n + 1)

radsToDegrees :: Float -> Int
radsToDegrees radians =
  let degrees = cycle [0 .. 359] -- cycle creates an infinitely repeating list
      converted = truncate $ (radians * 360) / (2 * pi)
   in degrees !! converted

{-
    360 deg = 2 * pi rad

    1 rad = 360 / (2 * pi) degress

    x rad = x * 360 / (2 * pi) degrees

    ghci> radsToDegrees $ 2 * pi
    0

    ghci> radsToDegrees $ 3 * pi
    180

    ghci> radsToDegrees $ 4 * pi
    0
-}

-- our own version of cycle: epicCycle

epicCycle inputList =
  cycleHelper inputList
  where
    cycleHelper [] = epicCycle inputList
    cycleHelper (x : xs) = x : cycleHelper xs

moreEpicCycle inputList =
  inputList <> moreEpicCycle inputList

findFirst predicate =
    foldr findHelper []
    where
        findHelper listElement maybeFound
            | predicate listElement = [listElement]
            | otherwise = maybeFound

{-
    foldr func carryValue lst

    In findFirst,
        func is findHelper
        carryvalue is  []
    and
        foldr is partially applied (lst to be supplied when findFirst is evaluated)

    foldr definition:

    foldr func carryValue lst =
        if null lst
        then carryValue
        else func (head lst) $ foldr func carryValue (tail lst)

    From the definition of foldr we can see that,

        foldr only evaluates maybeFound when the predicate is False. 
        When the predicate is True, it returns [listElement] immediately, 
        ignoring maybeFound.

        Note:   
        
        maybeFound 
        is equal to 
        foldr func carryValue (tail lst)
        
        This means the function doesn’t need to process the entire list after finding a match.


    ---------------------------------------------      

    findFirst (> 5) [1..100]
    [6]

    findFirst (> 10) [1..]
    [11]

    findFirst (> 50) (cycle [1..100])
    [51]

    findFirst (> 100) [1..10]
    []

    findFirst (> 10) (cycle [1..5]) 
    Interrupted.
-}

-- infinite fibonacci numbers

fib n 
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = (fib $ n - 1) + (fib $ n - 2)

-- fibs: stream of fibonacci numbers

fibs = map fib [0..]

smallFibs = takeWhile (< 100) fibs
-- [0,1,1,2,3,5,8,13,21,34,55,89]

-- Creating Lazy Streams

fibs' firstFib secondFib =
    let nextFib = firstFib + secondFib
    in firstFib : fibs' secondFib nextFib

fibs'' = 0 : 1 : helper fibs'' (tail fibs'')
    where
        helper (a:as) (b:bs) =
            a + b : helper as bs

main = putStrLn $ show $ "Hello, World!"