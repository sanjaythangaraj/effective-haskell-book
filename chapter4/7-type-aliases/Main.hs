module Main where

type Meters = Double
type Seconds = Double
type MetersPerSecond = Double

velocity :: Meters -> Seconds -> MetersPerSecond
velocity meters seconds = meters / seconds

speedLimit :: Double
speedLimit =
    let
        meters = 299792458 :: Double
        seconds = 1.0 :: Double
    in velocity meters seconds

-- Using Type Aliases with Type Parameters

data AppError = AppError {
    errorMessage :: String,
    errorContext :: [String],
    errorWrapped :: Maybe AppError
}

data Order = Order
data Invoice = Invoice
data Widget = Widget

-- before using type aliases

parseUserOrder' = undefined :: String -> Either AppError Order
generateInvoice' = undefined :: Either AppError Invoice
updateInventory' = undefined :: [Order] -> [(Widget, Int)] -> Either AppError [(Widget, Int)]

-- using type aliaes

-- type AppValue a = Either AppError a

-- η-reduce

type AppValue = Either AppError

parseUserOrder  = undefined :: String -> AppValue Order
generateInvoice = undefined :: Order -> AppValue Invoice
updateInventory = undefined :: [Order] -> [(Widget, Int)] -> AppValue [(Widget, Int)]

main = putStrLn $ show $ "Hello, World"