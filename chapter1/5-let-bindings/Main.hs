module Main where

makeGreeting salutation person =
  let messageWithTrailingSpace = salutation <> " "
   in messageWithTrailingSpace <> person

extendedGreeting person =
  let hello = makeGreeting "Hello" person
      goodDay = makeGreeting "I hope you have a nice afternoon" person
      goodBye = makeGreeting "See you later" person
   in hello <> "\n" <> goodDay <> "\n" <> goodBye

extendedGreeting' person =
  let hello = makeGreeting helloStr person
      goodDay = makeGreeting "I hope you have a nice afternoon" person
      goodBye = makeGreeting "See you later" person
      helloStr = "Hello"
   in hello <> "\n" <> goodDay <> "\n" <> goodBye

extendedGreeting'' person =
  let joinWithNewlines a b = a <> "\n" <> b
      hello = makeGreeting "Hello" person
      goodbye = makeGreeting "Goodbye" person
   in joinWithNewlines hello goodbye

extendedGreeting''' person =
  let joinWithNewlines a b = a <> "\n" <> b
      joined = joinWithNewlines hello goodbye
      hello = makeGreeting "Hello" person
      goodbye = makeGreeting "Goodbye" person
   in joined

extendedGreeting'''' person =
    let joinWithNewlines a b = a <> "\n" <> b
        helloAndGoodbye hello goodbye =
            let hello' = makeGreeting hello person
                goodbye' = makeGreeting goodbye person
            in joinWithNewlines hello' goodbye'
    in helloAndGoodbye "Hello" "Goodbye"

letWhereGreeting name place =
    let
        salutation = "Hello " <> name
        meetingInfo = location "Tuesday"
    in salutation <> "\n" <> meetingInfo
    where
        location day = "we met at " <> place <> " on a " <> day

extendedGreetingWhere person =
    helloAndGoodbye "Hello" "Goodbye"
    where
        helloAndGoodbye hello goodbye =
            joinWithNewlines hello' goodbye'
            where
                hello' = makeGreeting hello person
                goodbye' = makeGreeting goodbye person
        joinWithNewlines a b = a <> "\n" <> b

main = putStrLn $ extendedGreetingWhere "Jane"