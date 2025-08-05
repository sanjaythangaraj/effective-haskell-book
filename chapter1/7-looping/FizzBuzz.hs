module FizzBuzz where

fizzBuzzFor number
  | 0 == number `rem` 15 = "fizzbuzz"
  | 0 == number `rem` 5 = "buzz"
  | 0 == number `rem` 3 = "fizz"
  | otherwise = show number

naiveFizzBuzz n i str =
  if i > n
    then str
    else
      let nextStr = str <> fizzBuzzFor i <> " "
       in naiveFizzBuzz n (i + 1) nextStr