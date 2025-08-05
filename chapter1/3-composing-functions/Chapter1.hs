module Chapter1 where

addOne num = num + 1

timesTwo num = num * 2

squared num = num * num

minusFive num = num - 5

findResult num = minusFive (squared (timesTwo (addOne num)))

doubleIncremented = addOne . addOne

makeGreeting salutation person = salutation <> " " <> person

-- η-reduction
makeGreeting' salutation = ((salutation <> " ") <>)

-- η-reduction - second call to (<>) as a prefix function
makeGreeting'' salutation = (<>) (salutation <> " ")

-- η-reduction - anonymous
makeGreeting''' = (<>) . (\salutation -> salutation <> " ")

-- η-reduction - pointfree
makeGreeting'''' = (<>) . (<> " ")