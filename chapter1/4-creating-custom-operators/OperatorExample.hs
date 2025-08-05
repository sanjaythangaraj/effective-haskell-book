module OperatorExample where

infixl 6 +++

(+++) a b = a + b

divide = (/)

infixl 7 `divide`