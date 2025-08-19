{-# LANGUAGE RecordWildCards #-}

module Main where

data CustomerInfo = CustomerInfo
  { firstName :: String,
    lastName :: String,
    widgetCount :: Int,
    balance :: Int
  }

-- customerGeorge :: CustomerInfo
-- customerGeorge =
--   CustomerInfo
--     { balance = 100,
--       lastName = "Bird",
--       firstName = "George",
--       widgetCount = 10
--     }

customerGeorge :: CustomerInfo
customerGeorge =
    let firstName = "George"
        lastName = "Bird"
        widgetCount = 10
        balance = 100
    in CustomerInfo {..}

-- customerFactory :: String -> String -> CustomerInfo
-- customerFactory fname lastName =
--   CustomerInfo
--     { balance = 0,
--       widgetCount = 5,
--       firstName = fname,
--       lastName = lastName
--     }

customerFactory firstName lastName =
    let widgetCount = 10
        balance = 100
    in CustomerInfo {..}

totalWidgetCount :: [CustomerInfo] -> Int
totalWidgetCount =
  sum . map widgetCount

-- record update syntax

emptyCart :: CustomerInfo -> CustomerInfo
emptyCart customer =
  customer
    { widgetCount = 0,
      balance = 0
    }

{-

ghci> george = emptyCart customerGeorge
ghci> balance george
0
ghci> balance customerGeorge
100

-}

showCustomer :: CustomerInfo -> String
showCustomer CustomerInfo{..} =
    firstName
    <> " "
    <> lastName
    <> " "
    <> show widgetCount
    <> " "
    <> show balance

main = putStrLn $ showCustomer customerGeorge