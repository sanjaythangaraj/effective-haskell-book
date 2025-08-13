module Main where
import Prelude hiding (fst, snd)

-- by using patterns for your function arguments you can create different implementations
-- of your function for different inputs.

customGreeting "George" = "Oh, hey George!"
customGreeting name = "Hello, " <> name

matchTuple ("hello", "world") = "Hello there, you great big world"
matchTuple ("hello", name) = "Oh, hi there, " <> name
matchTuple (salutation, "George") = "Oh! " <> salutation <> " George!"
matchTuple n = show n

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

primes = 0 : sieve [2..]
    where
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

fancyNumbers n = (zip fibs primes) !! n

-- fancyNumbers 27
-- (317811,103)


-- printFancy does pattern matching on tuple
printFancy n = 
    let (fib, prime) = fancyNumbers n
        fib' = show fib
        prime' = if prime == 0 then "NIL" else show prime
    in "The fibonnacci number is: " <> fib' <> " and the prime is: " <> prime'

-- printFancy 27
-- output: The fibonnacci number is: 317811 and the prime is: 103

-- printFancy 0
-- output: The fibonnacci number is: 1 and the prime is: NIL

-- printFancy 1
-- output: The fibonnacci number is: 1 and the prime is: 2

-- modifyPair matches the entire element along with a pattern
modifyPair p@(a,b)
    | a == "Hello" = "this is a salutation"
    | b == "George" = "this is a message for George"
    | otherwise = "I don't know what " <> show p <> " means"

-- wildcard pattern

fst (x, _, _) = x
snd (_, x, _) = x
thrd (_, _, x) = x

-- map ($ (1, 2, 3)) [fst, snd, thrd]
-- [1,2,3]


-- '_tail' is to communicate that the value is the tail of a list,
-- and also that it's not a value we are going to be using

printHead [] = "empty!"
printHead lst@(head:_tail) =
    "the head of " <> (show lst) <> " is " <> show head


-- a case statement allows you to pattern match on a value inside of your function

favoriteFood person =
    case person of
        "Ren"       ->  "Tofu"
        "Rebecca"   ->  "Falafel"
        "George"    ->  "Banana"
        name        -> "I Don't know what " <> name <> " likes!"

-- combining case statements with guards

handleNums l =
    case l of
        [] -> "An empty list"
        [x] | x == 0 -> "a list called: [0]"
            | x == 1 -> "a singular list of [1]"
            | even x -> "a singleton list containig an even number"
            | otherwise -> "the list contains " <> (show x)
        _list -> "the list has more than 1 element"

-- getting warned about incomplete (non-exhaustive) patterns


-- turn on compiler warnings
-- ghci> :set -Wincomplete-patterns

--  Main.hs:89:1: warning: [GHC-62161] [-Wincomplete-patterns]
--     Pattern match(es) are non-exhaustive
--     In an equation for `partialFunction':
--         Patterns of type `a' not matched: p where p is not one of {0}
--    |
-- 89 | partialFunction 0 = "I only work for 0"

-- turn off compiler warnings
-- :set -Wno-incomplete-patterns

partialFunction 0 = "I only work for 0"


-- partialFunction impossibleValue = error $
--     "I only work with 0 but I was called with " <> show impossibleValue

-- *** Exception: I only work with 0 but I was called with 10
-- CallStack (from HasCallStack):
--   error, called at Main.hs:90:35 in main:Main

main = putStrLn $ show $ "Hello, World!"