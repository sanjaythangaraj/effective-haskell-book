module Main where

double = [2 * number | number <- [0..10]]

doubleOdds = [2 * number | number <- [0..10], odd number]
doubleOdds' = map (\number -> 2 * number) . filter odd $ [0..10]

pairs as bs =
    let as' = filter (`elem` bs) as
        bs' = filter odd bs
        mkPairs a = map (\b -> (a,b)) bs'
    in concat $ map mkPairs as'

pairs' as bs =
    [(a, b) | a <- as, b <- bs, a `elem` bs, odd b]

-- pairs' [1..10] [2..5]

combineLists as bs =
    let
        a = head as
        b = head bs
        as' = tail as
        bs' = tail bs
    in if null as || null bs
        then []
        else (a, b) : combineLists as' bs'

-- combineLists [1..5] ["I", "II", "III", "IV", "V", "VI", "VII"]
-- output: [(1,"I"),(2,"II"),(3,"III"),(4,"IV"),(5,"V")]

pairwiseSum xs ys =
   let sumElems pairs =
        let a = fst pairs
            b = snd pairs
        in a + b
    in map sumElems $ zip xs ys

-- pairwiseSum [1..5] [6..10]
-- output: [7,9,11,13,15]

pairwiseSum' xs ys = map (uncurry (+)) $ zip xs ys

-- pairwiseSum' [1..5] [6..10]
-- output: [7,9,11,13,15]

main = print $ pairwiseSum' [1..5] [6..10]