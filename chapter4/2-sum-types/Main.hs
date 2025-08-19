module Main where

data Bool = True | False

data Direction = North | South | East | West

-- sum of products

data PreferredContactMethod
  = Email String
  | TextMessage String
  | Mail String String String Int

emailContact :: PreferredContactMethod
emailContact = Email "me@example.com"

textContact :: PreferredContactMethod
textContact = TextMessage "+1 307 555 0100"

mailContact :: PreferredContactMethod
mailContact = Mail "1123 S. Road St." "Suite 712" "Examplesville, OH" 98142

-- pattern matching

confirmContact :: PreferredContactMethod -> String
confirmContact contact =
  case contact of
    Email emailAddress ->
      "Okay, I'll email you at " <> emailAddress
    TextMessage phoneNumber ->
      "Okay, I'll text you at " <> phoneNumber
    Mail street1 street2 citystate zip ->
      "Okay, I'll send a letter to\n"
        <> street1
        <> "\n"
        <> street2
        <> "\n"
        <> citystate
        <> " "
        <> show zip

confirmContact' :: PreferredContactMethod -> String
confirmContact' contact =
  case contact of
    Mail {} -> "Okay, I'll send you a letter!"
    Email {} -> "Okay, I'll email you!"
    TextMessage {} -> "Okay, I'll text you!"

data StringOrNumber = S String | N Int

stringsAndNumbers :: [StringOrNumber]
stringsAndNumbers = [S "This list has", N 2, S "different types of values"]

-- Summing Records

-- data Person
--   = Customer
--       { name :: String,
--         balance :: Int
--       }
--   | Employee
--       { name :: String,
--         managerName :: String,
--         salary :: Int
--       }

-- george :: Person
-- george = Customer {
--     name = "Georgie Bird",
--     balance = 100
-- }

-- porter :: Person
-- porter = Employee {
--     name = "Porter P. Pupper",
--     managerName = "Remi",
--     salary = 10
-- }

{-
    ghci> balance george
    100
    ghci> balance porter
    *** Exception: No match in record selector balance
    ghci> managerName porter
    "Remi"
    ghci> managerName george
    "*** Exception: No match in record selector managerName
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

getPersonName :: Person -> String
getPersonName person =
  case person of
    Employee employee -> employeeName employee
    Customer customer -> customerName customer

-- ghci> getPersonName george
-- "George Bird"
-- ghci> getPersonName porter
-- "Porter P. Pupper"

data MaybeString = NoString | SomeString String
  deriving (Show)

getPersonManager :: Person -> MaybeString
getPersonManager person =
  case person of
    Employee employee -> SomeString $ employeeManagerName employee
    Customer _ -> NoString

-- ghci> getPersonManager porter
-- SomeString "Remi"
-- ghci> getPersonManager george
-- NoString

data MaybeInt = NoInt | SomeInt Int
  deriving (Show)

getPersonBalance :: Person -> MaybeInt
getPersonBalance person =
  case person of
    Customer customer -> SomeInt $ customerBalance customer
    Employee _ -> NoInt

getPersonSalary :: Person -> MaybeInt
getPersonSalary person =
  case person of
    Employee employee -> SomeInt $ employeeSalary employee
    Customer _ -> NoInt

-- ghci> getPersonBalance george
-- SomeInt 100
-- ghci> getPersonBalance porter
-- NoInt
-- ghci>
-- ghci> getPersonSalary porter 
-- SomeInt 10
-- ghci> getPersonSalary george
-- NoInt

main = putStrLn $ show $ "Hello, World"