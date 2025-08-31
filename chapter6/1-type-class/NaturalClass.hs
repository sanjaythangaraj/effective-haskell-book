module NaturalClass where

class Natural' n where
  equal' :: n -> n -> Bool
  add' :: n -> n -> n
  multiply' :: n -> n -> n
  additiveIdentity' :: n
  multiplicativeIdentity' :: n
  displayAsString' :: n -> String

instance Natural' Int where
  equal' = (==)
  add' = (+)
  multiply' = (*)
  additiveIdentity' = 0
  multiplicativeIdentity' = 1
  displayAsString' = show

class (Show n, Eq n) => Natural n where
  add :: n -> n -> n
  multiply :: n -> n -> n
  additiveIdentity :: n
  multiplicativeIdentity :: n

instance Natural Int where
  add = (+)
  multiply = (*)
  additiveIdentity = 0
  multiplicativeIdentity = 1

data Peano = Z | S Peano

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S $ toPeano (n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S p) = 1 + fromPeano p

instance Natural Peano where
  add Z a = a
  add (S a) b = add a (S b)
  multiply Z _ = Z
  multiply (S a) b = add b (multiply a b)
  additiveIdentity = Z
  multiplicativeIdentity = S Z

instance Eq Peano where
  (==) Z Z = True
  (==) (S a) (S b) = a == b
  (==) _ _ = False

instance Show Peano where
  show Z = "Z"
  show (S a) = "(S " <> show a <> ")"

-- ghci> add (3 :: Int) (2 :: Int)
-- 5
-- ghci> multiply (3 :: Int) (2 :: Int)
-- 6
-- ghci> fromPeano $ add (toPeano 3) (toPeano 2)
-- 5
-- ghci> fromPeano $ multiply (toPeano 3) (toPeano 2)
-- 6
