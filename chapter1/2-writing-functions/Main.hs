module Main where

makeGreeting salutation person =
  salutation <> " " <> person

enthusiasticGreeting salutation =
    makeGreeting (salutation <> "!")

makeEnthusiasticGreeting salutation name =
    makeGreeting (salutation <> "!") name

greetGeorge = (`makeGreeting` "George")

greetJane = flip makeGreeting "Jane"

sayThree a b c = a <> " " <> b <> " " <> c

main = print "no salutation to show yet"