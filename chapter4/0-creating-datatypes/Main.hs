module Main where

data CustomerInfo = CustomerInfo String String Int Int

customerGeorge :: CustomerInfo
customerGeorge = CustomerInfo "Georgie" "Bird" 10 100

showCustomer :: CustomerInfo -> String
showCustomer (CustomerInfo first last count balance) =
    let fullName = first <> " " <> last
        name = "name: " <> fullName
        count' = "count: " <> (show count)
        balance' = "balance: " <> (show balance)
    in name <> " " <> count' <> " " <> balance'

applyDiscount :: CustomerInfo -> CustomerInfo
applyDiscount customer = 
    case customer of
        (CustomerInfo "Georgie" "Bird" count balance) ->
            CustomerInfo "Georgie" "Bird" count (balance `div` 4)
        (CustomerInfo "Porter" "Pupper" count balance) ->
            CustomerInfo "Porter" "Pupper" count (balance `div` 2)
        otherCustomer -> otherCustomer

firstName :: CustomerInfo -> String
firstName (CustomerInfo name _ _ _) = name

lastName :: CustomerInfo -> String
lastName (CustomerInfo _ name _ _) = name

widgetCount :: CustomerInfo -> Int
widgetCount (CustomerInfo _ _ count _) = count

balance :: CustomerInfo -> Int
balance (CustomerInfo _ _ _ balance) = balance

updateFirstName :: CustomerInfo -> String -> CustomerInfo
updateFirstName (CustomerInfo _ lastName count balance) firstName =
    CustomerInfo firstName lastName count balance

updateLastName :: CustomerInfo -> String -> CustomerInfo
updateLastName (CustomerInfo firstName _ count balance) lastName =
    CustomerInfo firstName lastName count balance

updateWidgetCount :: CustomerInfo -> Int -> CustomerInfo
updateWidgetCount (CustomerInfo firstName lastName _ balance) count =
    CustomerInfo firstName lastName count balance

updateBalance :: CustomerInfo -> Int -> CustomerInfo
updateBalance (CustomerInfo firstName lastName count _) balance =
    CustomerInfo firstName lastName count balance


main = putStrLn $ show $ "Hello World"