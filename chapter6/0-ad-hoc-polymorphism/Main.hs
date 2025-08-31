module Main where

-- unique - remove duplicate elements from a list.

-- unique using parametric polymorphism
--  need to pass in a function to let us test equality.

unique :: (a -> a -> Bool) -> [a] -> [a]
unique _ [] = []
unique f (elem : elems) =
  let f' a b = not $ f a b
      elems' = filter (f' elem) elems
   in elem : unique f elems'

-- ghci> unique (==) [1,2,3,2,1]
-- [1,2,3]
-- ghci> unique (==) ["hello", "george", "george", "hello"]
-- ["hello","george"]

-- sumOfUniques - sum of the unique numbers in a list

-- sumOfUniques using parametric polymorphism
--  need to accept another function to add the elements,
--  as well as a default value

sumOfUniques ::
  (a -> a -> a) ->
  (a -> a -> Bool) ->
  a ->
  [a] ->
  a
sumOfUniques add compare zero =
  foldr add zero . unique compare

-- a record to hold a set of related functions,
-- allowing to pass around a single parameter

data Natural a = Natural
  { equal :: a -> a -> Bool,
    add :: a -> a -> a,
    multiply :: a -> a -> a,
    additiveIdentity :: a,
    multiplicativeIdentity :: a,
    displayAsString :: a -> String
  }

intNatural :: Natural Int
intNatural =
  Natural
    { equal = (==),
      add = (+),
      multiply = (*),
      additiveIdentity = 0,
      multiplicativeIdentity = 1,
      displayAsString = show
    }

data Peano = Z | S Peano
    deriving Show

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S $ toPeano (n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S p) = 1 + fromPeano p

peanoNatural :: Natural Peano
peanoNatural =
  Natural
    { equal = comparePeano,
      add = addPeano,
      multiply = multiplyPeano,
      additiveIdentity = Z,
      multiplicativeIdentity = S Z,
      displayAsString = show . fromPeano
    }
  where
    comparePeano Z Z = True
    comparePeano (S a) (S b) = comparePeano a b
    comparePeano _ _ = False
    addPeano Z b = b
    addPeano (S a) b = addPeano a (S b)
    multiplyPeano Z _ = Z
    multiplyPeano (S a) b =
      addPeano b (multiplyPeano a b)

unique' :: Natural a -> [a] -> [a]
unique' _ [] = []
unique' n (elem:elems) = 
    let
        compare a b = not $ (equal n) a b
        elems' = filter (compare elem) elems
    in elem : unique' n elems'

-- ghci> unique' intNatural [1,2,2,3,2,1]
-- [1,2,3]

sumOfUniques' :: Natural a -> [a] -> a
sumOfUniques' n =
    foldr (add n) (additiveIdentity n) . unique' n

-- ghci> unique' peanoNatural [toPeano 1, toPeano 1, toPeano 2, toPeano 2, toPeano 3, toPeano 2, toPeano 1]
-- [S Z,S (S Z),S (S (S Z))]

main = putStrLn $ show $ "Hello, World"