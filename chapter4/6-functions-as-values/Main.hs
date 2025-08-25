module Main where

-- Functions as Values

-- a data type that holds a function from a String to a pair
-- of strings
data StringParser = StringParser (String -> (String, String))

data StringParser'
  = StringParser'
  { runStringParser :: String -> (String, String)
  }

takeCharacters'''' :: Int -> (String -> (String, String))
takeCharacters'''' numCharacters inputString =
    splitAt numCharacters inputString

takeCharacters' :: Int -> (String -> (String, String))
takeCharacters' numCharacters = stringParser
  where
    stringParser :: String -> (String, String)
    stringParser inputString =
      splitAt numCharacters inputString

takeCharacters'' :: Int -> (String -> (String, String))
takeCharacters'' numCharacters = stringParser
  where
    stringParser :: String -> (String, String)
    stringParser = \inputString ->
      splitAt numCharacters inputString

takeCharacters''' :: Int -> StringParser
takeCharacters''' numCharacters = StringParser stringParser
  where
    stringParser :: String -> (String, String)
    stringParser = \inputString ->
      splitAt numCharacters inputString

takeCharacters :: Int -> StringParser'
takeCharacters numCharacters = StringParser' $ \inputString ->
  splitAt numCharacters inputString

-- ghci> parseString (takeCharacters 3) "apple pie"
-- "app"

getNextWord :: StringParser'
getNextWord = StringParser' $ \someString ->
  case break (== ' ') someString of
    (nextWord, "") -> (nextWord, "")
    (nextWord, rest) -> (nextWord, tail rest)


-- ghci> parseString getNextWord "Hello World"
-- "Hello"

combineParsers :: StringParser' -> StringParser' -> StringParser'
combineParsers firstParser secondParser = StringParser' $ \someString ->
  let (_firstPart, firstResult) = runStringParser firstParser someString
  in runStringParser secondParser firstResult

parseString :: StringParser' -> String -> String
parseString parser inputString =
  fst $ runStringParser parser inputString

-- ------------------------

getNextWordAfterTenLetters :: StringParser'
getNextWordAfterTenLetters =
  combineParsers (takeCharacters 10) getNextWord

-- ghci> parseString getNextWordAfterTenLetters "Hello, World"   
-- "ld"

tenLettersAfterTheFirstWord :: StringParser'
tenLettersAfterTheFirstWord =
  combineParsers getNextWord (takeCharacters 10)

-- ghci> parseString tenLettersAfterTheFirstWord "Welcome. Hello Moon :)" 
-- "Hello Moon"

-- -------------------------

-- ghci> secondWord = combineParsers getNextWord getNextWord
-- ghci> thirdWord = combineParsers getNextWord secondWord
-- ghci> parseString thirdWord "one two three four five"
-- "three"


main = putStrLn $ show $ "Hello, World"