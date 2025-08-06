module Main where

-- import Prelude hiding (foldl, foldr)

doubleElems :: [Int] -> [Int]
doubleElems nums =
  if null nums
    then []
    else (2 * (head nums)) : doubleElems (tail nums)

-- foldr func carryValue lst =
--     if null lst
--     then carryValue
--     else func (head lst) $ foldr func carryValue (tail lst)

doubleElems' elems = foldr doubleElem [] elems
  where
    doubleElem num lst = (2 * num) : lst

doubleElems'' elems = foldr (applyElem (* 2)) [] elems
  where
    applyElem f elem accumulator = f elem : accumulator

map' f = foldr (applyElem f) []
  where
    applyElem f elem accumulator = f elem : accumulator

doubleWithMap elems = map' (* 2) elems

map'' f xs =
  if null xs
    then []
    else f (head xs) : map'' f (tail xs)

-- filtering list elements

checkGuestList guestList name =
  name `elem` guestList

foodCosts = [("Ren", 10.00), ("George", 4.00), ("Porter", 27.50)]

partyBudget isAttending =
    foldr (+) 0 . map snd . filter (isAttending . fst)

-- partyBudget (checkGuestList ["Ren", "Porter"]) foodCosts

main = print "Hello World"