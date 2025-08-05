module Main where
-- import Prelude hiding (foldl, foldr)

doubleElems :: [Int] -> [Int]
doubleElems nums =
    if null nums then []
    else (2 * (head nums)) : doubleElems (tail nums)

-- foldr func carryValue lst =
--     if null lst
--     then carryValue
--     else func (head lst) $ foldr func carryValue (tail lst)

doubleElems' elems = foldr doubleElem [] elems
    where
        doubleElem num lst = (2 * num) : lst

doubleElems'' elems = foldr (applyElem (*2)) [] elems
    where
        applyElem f elem accumulator = f elem : accumulator

map' f = foldr (applyElem f) []
    where
        applyElem f elem accumulator = f elem : accumulator

doubleWithMap elems = map' (*2) elems

map'' f xs =
    if null xs then []
    else f (head xs) : map'' f (tail xs)

main = print "Hello World"