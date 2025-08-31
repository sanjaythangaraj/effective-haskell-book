module OrdExample where

import Data.Word (Word8)
import Prelude hiding (Ord (..), Ordering (..))

data Ordering = LT | EQ | GT

instance Show Ordering where
  show LT = "LT"
  show EQ = "EQ"
  show GT = "GT"

-- Type class without any default implementations

-- class (Eq a) => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a

-- instance Ord Word8 where
--   compare a b
--     | a == b = EQ
--     | a == 0 = LT
--     | b == 0 = GT
--     | otherwise = compare (a - 1) (b - 1)
--   a < b =
--     case compare a b of
--       LT -> True
--       _ -> False
--   a <= b =
--     case compare a b of
--       GT -> False
--       _ -> True
--   a > b =
--     case compare a b of
--       GT -> True
--       _ -> False
--   a >= b =
--     case compare a b of
--       LT -> False
--       _ -> True
--   max a b =
--     case compare a b of
--       GT -> a
--       _ -> b
--   min a b =
--     case  compare a b of
--       LT -> a
--       _ -> b


-- Type class Ord with default implementation of some functions.
--  The default implementations are defined in terms of compare.
--  The type class definition doesn't have a default implementation for compare.
--  When defining a type class instance, the user needs to implement
--  the compare.

-- class (Eq a) => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   a < b =
--     case compare a b of
--       LT -> True
--       _ -> False
--   (<=) :: a -> a -> Bool
--   a <= b =
--     case compare a b of
--       GT -> False
--       _ -> True
--   (>) :: a -> a -> Bool
--   a > b =
--     case compare a b of
--       GT -> True
--       _ -> False
--   (>=) :: a -> a -> Bool
--   a >= b =
--     case compare a b of
--       LT -> False
--       _ -> True
--   max :: a -> a -> a
--   max a b =
--     case compare a b of
--       GT -> a
--       _ -> b
--   min :: a -> a -> a
--   min a b =
--     case compare a b of
--       LT -> a
--       _ -> b

-- instance Ord Word8 where
--   compare a b
--     | a == b = EQ
--     | a == 0 = LT
--     | b == 0 = GT
--     | otherwise = compare (a - 1) (b - 1)


-- ghci> compare (1 :: Word8) (0 :: Word8)
-- GT    
-- ghci> (1 :: Word8) == (1 :: Word8)
-- True
-- ghci> max (3 :: Word8) (5 :: Word8)
-- 5
-- ghci> min (3 :: Word8) (5 :: Word8)
-- 3


-- Type class Ord with default implementations; default implementation for compare in terms of (<=).
--  The default implementations are defined in terms of compare.
--  When defining a type class instance, the user needs to implement
--  the compare or (<=), which is specified using the MINIMAL pragma.


class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  compare a b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT
  (<) :: a -> a -> Bool
  a < b =
    case compare a b of
      LT -> True
      _ -> False
  (<=) :: a -> a -> Bool
  a <= b =
    case compare a b of
      GT -> False
      _ -> True
  (>) :: a -> a -> Bool
  a > b =
    case compare a b of
      GT -> True
      _ -> False
  (>=) :: a -> a -> Bool
  a >= b =
    case compare a b of
      LT -> False
      _ -> True
  max :: a -> a -> a
  max a b =
    case compare a b of
      GT -> a
      _ -> b

  min :: a -> a -> a
  min a b =
    case compare a b of
      LT -> a
      _ -> b
  {-# MINIMAL compare | (<=) #-}

instance Ord Word8 where
  -- compare a b
  --   | a == b = EQ
  --   | a == 0 = LT
  --   | b == 0 = GT
  --   | otherwise = compare (a - 1) (b - 1)
  (<=) a b
    | a == b = True
    | a == 0 = True
    | b == 0 = False
    | otherwise = (a - 1) <= (b-1)

-- ghci> compare (1 :: Word8) (0 :: Word8)
-- GT
-- ghci> max (3 :: Word8) (5 :: Word8)
-- 5
-- ghci> min (3 :: Word8) (5 :: Word8)
-- 3