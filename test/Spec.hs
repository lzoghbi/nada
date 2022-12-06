import Test.Nada.Calendar

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [calendarTests]
