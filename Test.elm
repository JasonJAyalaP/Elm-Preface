module Test where

import open Either
import open Preface

data Test a a = Test String a a
type Grade = Either String String

grade : Test -> Grade
grade (Test name x y) = if | x == y    -> Left <| name ++ " passed."
                           | otherwise -> Right <| name ++ "\nExpected: " ++ show y ++ "\n     Got: " ++ show x ++ "\n"

try : String -> a -> a -> Grade
try s x y = grade <| Test s x y

format : [Grade] -> [Element]
format grades = let errors = rights grades in
                if | isEmpty errors -> [plainText "All Tests Passed."]
                   | otherwise      -> plainText "---FAILED---\n" :: (map (text . monospace . toText) errors)

main = flow down <| format grades

grades : [Grade]
grades =
  [ try "isEmpty" (isEmpty []) True
  , try "isEmpty" (isEmpty [1]) False
  , try "isEmpty" (isEmpty [{x=1,y=2},{x=3,y=4}]) False
  , try "repeat" (repeat 42 3) [42,42,42]
  , try "repeat" (repeat "Elm" 1) ["Elm"]
  , try "repeat" (repeat "No Elm" 0) []
  , try "replicate" (replicate 2 "No") (repeat "No" 2)
  , try "isOdd 0" (isOdd 0) False
  , try "isOdd -1" (isOdd (0-1)) True
  , try "isOdd 5" (isOdd 5) True
  , try "isOdd 32" (isOdd 32) False
  , try "isEven 0" (isEven 0) True
  , try "isEven -2" (isEven (0-2)) True
  , try "isEven 32" (isEven 32) True
  , try "isEven 5" (isEven 5) False
  , try "takeWhile []" (takeWhile isOdd []) []
  , try "takeWhile isOdd" (takeWhile isOdd [1,3,4,5]) [1,3]
  , try "takeWhile no pass" (takeWhile isOdd [2,1]) []
  , try "takeWhile isOdd" (takeWhile isOdd [2]) []
  , try "dropWhile []" (dropWhile isEven []) []
  , try "dropWhile isEven" (dropWhile isEven [2,4,5,2]) [5,2]
  , try "dropWhile no drop" (dropWhile isEven [3,5]) [3,5]
  , try "dropWhile drop all" (dropWhile isEven [2,4]) []
  , try "init" (init [1,2,3]) [1,2]
  , try "init []" (init []) []
  , try "init [Hi]" (init ["Hi"]) []
  , try "tail" (tail [1,2,3]) [2,3]
  , try "tail []" (tail []) []
  , try "tail [Hi]" (tail ["Hi"]) []
  , try "# string" ("Elm" # 3) (Just 'm')
  , try "# []" ([] # 1) Nothing
  , try "# -1" ("Elm" # (0-1)) Nothing
  , try "# overflow" ("Elm" # 99) Nothing
  , try "iterate []" (iterate not [] 0) []
  , try "iterate 1" (iterate not "Hi" 1) ["Hi"]
  , try "iterate *2" (iterate (\x->x*2) 1 5) [1,2,4,8,16]
  , try "iterate not 4" (iterate not True 4) [True, False, True, False]
  , try "cycle 0" (cycle "Elm" 0) []
  , try "cycle 1" (cycle "Elm" 1) "Elm"
  , try "cycle Elm" (cycle "Elm" 3) "ElmElmElm"
  , try "find isOdd" (find isOdd [2,4,3,7]) (Just 3)
  , try "find isEven" (find isEven [1,3,55]) Nothing
  , try "find []" (find isEven []) Nothing
  ]
