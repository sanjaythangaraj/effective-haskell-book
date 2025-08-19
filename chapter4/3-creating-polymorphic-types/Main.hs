module Main where

{-

Maybe

data Maybe a = Nothing | Just a

-}

data CustomerInfo = CustomerInfo
  { customerName :: String,
    customerBalance :: Int
  }

data EmployeeInfo = EmployeeInfo
  { employeeName :: String,
    employeeManagerName :: String,
    employeeSalary :: Int
  }

data Person
  = Customer CustomerInfo
  | Employee EmployeeInfo

george =
  Customer $
    CustomerInfo
      { customerName = "George Bird",
        customerBalance = 100
      }

porter =
  Employee $
    EmployeeInfo
      { employeeName = "Porter P. Pupper",
        employeeManagerName = "Remi",
        employeeSalary = 10
      }


getPersonManager :: Person -> Maybe String
getPersonManager person =
    case person of 
        Employee employeeInfo -> Just $ employeeManagerName employeeInfo
        _ -> Nothing

getPersonBalance :: Person -> Maybe Int
getPersonBalance person =
    case person of
        Customer customerInfo -> Just $ customerBalance customerInfo
        _ -> Nothing

getPersonSalary :: Person -> Maybe Int
getPersonSalary person =
    case person of
        Employee employeeInfo -> Just $ employeeSalary employeeInfo
        _ -> Nothing

-- ghci> getPersonSalary george
-- Nothing
-- ghci> getPersonSalary porter
-- Just 10

maybeToList :: Maybe a -> [a]
maybeToList (Just val) = [val]
maybeToList Nothing = []

{-

Either

data Either a b = Left a | Right b

-}


eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe e =
    case e of
        Left _ -> Nothing
        Right val -> Just val


handleMissingRight :: Either String (Maybe a) -> Either String a
handleMissingRight e =
    case e of
        Left err -> Left err
        Right (Just val) -> Right val
        Right Nothing -> Left "Missing value"


main = putStrLn $ show $ "Hello, World"