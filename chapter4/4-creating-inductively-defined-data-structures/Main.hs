module Main where

-- Peano Numbers

data Peano = Z | S Peano

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S (toPeano $ n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S p) = succ (fromPeano p)

eqPeano :: Peano -> Peano -> Bool
eqPeano p p' =
    case (p, p') of
        (Z, Z) -> True
        (S n, S n') -> eqPeano n n'
        _ -> False

addPeano :: Peano -> Peano -> Peano
addPeano Z b = b
addPeano (S a) b = addPeano a (S b)

{-

    ghci> let x = Z
    ghci> :sprint x
    x = Z

    ghci> let y = S (S (S (S (S Z))))
    ghci> :sprint y
    y = S (S (S (S (S Z))))


    ghci> fromPeano x
    0
    ghci> fromPeano y
    5

    ghci> eqPeano (toPeano 0) x
    True
    ghci> eqPeano (toPeano 5) y
    True

    ghci> fromPeano (addPeano y (S (S Z))) 
    7

-}

-- Inductively Defined Lists

data List a = Empty | Cons a (List a)
    deriving Show

-- toList :: [a] -> List a
-- toList [] = Empty
-- toList (x:xs) = Cons x (toList xs)

-- fromList :: List a -> [a]
-- fromList Empty = []
-- fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList = foldr Cons Empty

-- ghci> toList [1,2,3,4]
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr _ b Empty = b
listFoldr f b (Cons x xs) =
    f x $ listFoldr f b xs

fromList :: List a -> [a]
fromList = listFoldr (:) []

-- ghci> fromList (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))
-- [1,2,3,4]

listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl _ b Empty = b
listFoldl f b (Cons x xs) =
    listFoldl f (f b x) xs

fromList' :: List a -> [a]
fromList' = reverse . listFoldl (flip (:)) []

-- ghci> fromList' (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))
-- [1,2,3,4]

listHead :: List a -> Maybe a
listHead Empty = Nothing
listHead (Cons x _) = Just x

-- ghci> listHead (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))   
-- Just 1
-- ghci> listHead Empty                                    
-- Nothing

listTail :: List a -> List a
listTail Empty = Empty
listTail (Cons _ xs) = xs

-- ghci> listTail (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))
-- Cons 2 (Cons 3 (Cons 4 Empty))
-- ghci> listTail Empty                                    
-- Empty

listReverse :: List a -> List a
listReverse = listFoldl (flip Cons) Empty

-- ghci> listReverse (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))
-- Cons 4 (Cons 3 (Cons 2 (Cons 1 Empty)))

listMap :: (a -> b) -> List a -> List b
listMap _ Empty = Empty
listMap f (Cons x xs) =
    Cons (f x) $ listMap f xs

main = putStrLn $ show $ "Hello World"