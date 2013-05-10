module Test where

import Preface
import Either

data Test a a = Test String a a
type Grade = Either String String

grade : Test -> Grade
grade (Test name x y) = if | x == y    -> Left <| name ++ " passed."
                           | otherwise -> Right <| name ++ "\nExpected: " ++ show y ++ "\n     Got: " ++ show x ++ "\n"

format : [Grade] -> [Element]
format grades = let errors = rights grades in
                if | isEmpty errors -> [plainText "All Tests Passed."]
                   | otherwise      -> plainText "---FAILED---\n" :: (map (text . monospace . toText) errors)

main = flow down <| format grades

grades : [Grade]
grades =
  [ grade <| Test "isEmpty" (isEmpty []) True
  , grade <| Test "isEmpty" (isEmpty [1]) False
  , grade <| Test "isEmpty" (isEmpty [{x=1,y=2},{x=3,y=4}]) False
  , grade <| Test "repeat" (repeat 42 3) [42,42,42]
  , grade <| Test "repeat" (repeat "Elm" 1) ["Elm"]
  , grade <| Test "repeat" (repeat "No Elm" 0) []
  , grade <| Test "replicate" (replicate 2 "No") (repeat "No" 2)
  , grade <| Test "isOdd 0" (isOdd 0) False
  , grade <| Test "isOdd -1" (isOdd (0-1)) True
  , grade <| Test "isOdd 5" (isOdd 5) True
  , grade <| Test "isOdd 32" (isOdd 32) False
  , grade <| Test "isEven 0" (isEven 0) True
  , grade <| Test "isEven -2" (isEven (0-2)) True
  , grade <| Test "isEven 32" (isEven 32) True
  , grade <| Test "isEven 5" (isEven 5) False
  , grade <| Test "takeWhile []" (takeWhile isOdd []) []
  , grade <| Test "takeWhile isOdd" (takeWhile isOdd [1,3,4,5]) [1,3]
  , grade <| Test "takeWhile no pass" (takeWhile isOdd [2,1]) []
  , grade <| Test "takeWhile isOdd" (takeWhile isOdd [2]) []
  , grade <| Test "dropWhile []" (dropWhile isEven []) []
  , grade <| Test "dropWhile isEven" (dropWhile isEven [2,4,5,2]) [5,2]
  , grade <| Test "dropWhile no drop" (dropWhile isEven [3,5]) [3,5]
  , grade <| Test "dropWhile drop all" (dropWhile isEven [2,4]) []
  , grade <| Test "init" (init [1,2,3]) [1,2]
  , grade <| Test "init []" (init []) []
  , grade <| Test "init [Hi]" (init ["Hi"]) []
  , grade <| Test "tail" (tail [1,2,3]) [2,3]
  , grade <| Test "tail []" (tail []) []
  , grade <| Test "tail [Hi]" (tail ["Hi"]) []
  , grade <| Test "index string" ("Elm" `index` 3) (Just 'm')
  , grade <| Test "index []" ([] `index` 1) Nothing
  , grade <| Test "index -1" ("Elm" `index` (0-1)) Nothing
  , grade <| Test "index high index" ("Elm" `index` 99) Nothing
  ]

