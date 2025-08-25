module Main where

import Text.Read (readEither)

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show)

eval :: Expr -> Int
eval expr =
  case expr of
    Lit num -> num
    Add arg1 arg2 -> eval' (+) arg1 arg2
    Sub arg1 arg2 -> eval' (-) arg1 arg2
    Mul arg1 arg2 -> eval' (*) arg1 arg2
    Div arg1 arg2 -> eval' div arg1 arg2
  where
    eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Int
    eval' operator arg1 arg2 =
      operator (eval arg1) (eval arg2)

-- ghci> eval $ Add (Lit 1) (Lit 2)
-- 3
-- ghci> eval $ Sub (Lit 10) (Div (Lit 10) (Lit 2))
-- 5
-- ghci> eval $ Add (Lit 5) (Sub (Lit 10) (Div (Lit 10) (Lit 2)))
-- 10

-- ghci> eval $ Lit 5 `Add` (Lit 10 `Sub` (Lit 10 `Div` Lit 2))
-- 10

parse :: String -> Either String Expr
parse str =
  case parse' (words str) of
    Left err -> Left err
    Right (expr, []) -> Right expr
    Right (_, rest) -> Left $ "Found extra tokens: " <> (unwords rest)
  where
    parse' :: [String] -> Either String (Expr, [String])
    parse' [] = Left "unexpected end of expression"
    parse' (token : rest) =
      case token of
        "+" -> parseBinary Add rest
        "*" -> parseBinary Mul rest
        "-" -> parseBinary Sub rest
        "/" -> parseBinary Div rest
        lit ->
          case readEither lit of
            Left err -> Left err
            Right lit' -> Right (Lit lit', rest)
      where
        parseBinary :: (Expr -> Expr -> Expr) -> [String] -> Either String (Expr, [String])
        parseBinary exprConstructor args =
          case parse' args of
            Left err -> Left err
            Right (firstArg, rest') ->
              case parse' rest' of
                Left err -> Left err
                Right (secondArg, rest'') ->
                  Right $ (exprConstructor firstArg secondArg, rest'')

run :: String -> String
run expr =
  case parse expr of
    Left err -> "Error: " <> err
    Right expr' ->
        let answer = show $ eval expr'
        in "Parser Output: " <> (show expr') <> "\n" <>
            "The answer is: " <> answer

main = putStrLn $ show $ (run "+ 5 - 10 / 10 2")