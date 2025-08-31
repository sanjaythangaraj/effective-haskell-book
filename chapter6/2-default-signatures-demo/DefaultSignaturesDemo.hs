{-# LANGUAGE DefaultSignatures #-}
module DefaultSignaturesDemo where

data UserName = UserName String

instance Show UserName where
    show (UserName name) = name

-- note: instances of Show and Redacted' are identical

class Redacted' a where
    redacted' :: a -> String

instance Redacted' UserName where
    redacted' (UserName name) = name

-- requires a Show instance

class Show a => Redacted'' a where
    redacted'' :: a -> String
    redacted'' = show

instance Redacted'' UserName

-- DefaultSignatures language extension allows us to add a type signature 
--  (also with type class constraints) with the default keyword to the the 
--  default implementation of a function.
--  This means any instance that wants to provide a definition for a function which has
--  a type signature with default along with its general type signature only needs to satisfy
--  the general type.

class Redacted a where
    redacted :: a -> String
    default redacted :: Show a => a -> String
    redacted = show

instance Redacted UserName

data Password = Password String

instance Redacted Password where
    redacted _ = "<redacted>"

-- ghci> UserName "George"
-- George
-- ghci> redacted $ UserName "George"
-- "George"
-- ghci> redacted $ Password "hunter2"
-- "<redacted>"