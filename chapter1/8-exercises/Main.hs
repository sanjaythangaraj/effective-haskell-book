module Main where

-- factorial
factorial n
  | n <= 1 = 1
  | otherwise = n * factorial (n - 1)

-- fibonacci sequence
fibonacci n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

-- uncurried addition
uncurriedAddition nums =
  let a = fst nums
      b = snd nums
   in a + b

curriedAdd = curry uncurriedAddition
uncurriedAdd = uncurry (+)

curry' :: Num a => ((a, a) -> a) -> a -> a -> a
curry' f a b = f(a, b)

uncurry' :: Num a => (a -> a -> a) -> (a, a) -> a
uncurry' f (a,b) = f a b

curriedAdd' = curry' uncurriedAddition
uncurriedAdd' = uncurry' (+)


main = putStrLn $ "Hello World"