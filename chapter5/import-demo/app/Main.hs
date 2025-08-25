module Main where
import Data.Char

countNonPrintableCharacters :: String -> Int
countNonPrintableCharacters =
    length . filter (not . isPrint)

main :: IO ()
main = print $ countNonPrintableCharacters "\v\t\aHello\r\n"
