halveEvens :: [Integer] -> [Integer]
halveEvens = map (`quot` 2) . filter even

ex_halveEvens =
    [ halveEvens [] == []
    , halveEvens [1,2,3,4,5] == [1,2]
    , halveEvens [6,6,6,3,3,3,2,2,2] == [3,3,3,1,1,1]
    ]

safeString :: String -> String
safeString = undefined

ex_safeString =
    [ safeString [] == []
    , safeString "Hello World!" == "Hello World!"
    , safeString "That’s your line:\n" == "That_s your line:_"
    , safeString "🙋.o(“Me Me Me”)" == "_.o(_Me Me Me_)"
    ]

holes :: [a] -> [[a]]
holes = undefined

ex_holes =
   [ holes "" == []
   , holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
   ]

longestText :: Show a => [a] -> a
longestText = undefined

ex_longestText =
   [ longestText [True,False] == False
   , longestText [2,4,16,32] == (32::Int)
   , longestText (words "Hello World") == "World"
   , longestText (words "Olá mundo") ==  "Olá"
    ]

adjacents :: [a] -> [(a,a)]
adjacents = undefined

ex_adjacents =
   [ adjacents "" == []
   , adjacents [True] == []
   , adjacents "Hello" == [('H','e'),('e','l'),('l','l'),('l','o')]
   ]

commas :: [String] -> String
commas = undefined

ex_commas =
   [ commas [] == ""
   , commas ["Hello"] == "Hello"
   , commas ["Hello", "World"] == "Hello, World"
   , commas ["Hello", "", "World"] == "Hello, , World"
   , commas ["Hello", "new", "World"] == "Hello, new, World"
   ]

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials = undefined

ex_addPolynomials =
   [ addPolynomials [[]] == []
   , addPolynomials [[0, 1], [1, 1]] == [1, 2]
   , addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
   ]

sumNumbers :: String -> Integer
sumNumbers = undefined

ex_sumNumbers =
   [ sumNumbers "" == 0
   , sumNumbers "Hello world!" == 0
   , sumNumbers "a1bc222d3f44" == 270
   , sumNumbers "words0are1234separated12by3integers45678" == 46927
   , sumNumbers "000a." == 0
   , sumNumbers "0.00a." == 0
   ]

testResults :: [(String, [Bool])]
testResults = [ ("halveEvens",      ex_halveEvens)
              -- , ("safeString",      ex_safeString)
              -- , ("holes",           ex_holes)
              -- , ("longestText", ex_longestText)
              -- , ("adjacents", ex_adjacents)
              -- , ("commas", ex_commas) 
              -- , ("addPolynomials", ex_addPolynomials)
              -- , ("sumNumbers", ex_sumNumbers)
              ]

formatTests :: [(String, [Bool])] -> String
formatTests = unlines . map fmt 
    where fmt :: (String, [Bool]) -> String
          fmt (name, res) = name 
                         ++ ": Passed " 
                         ++ show (nPassed res)
                         ++ "/" 
                         ++ show (nTotal res)
                         ++ failedStr res
          
          nPassed res = length $ filter id res
          nTotal res = length res


          failedStr :: [Bool] -> String
          failedStr res
            | all id res = "."
            | all not res = ".  All test failed."
            | otherwise = "Failed tests ... " ++ (failedIdxStr res)

          failedIdxStr :: [Bool] -> String
          failedIdxStr res = unwords $ map show (failedIdxs res)

          failedIdxs :: [Bool] -> [Integer]
          failedIdxs res = map fst . zip [1..] $ filter (not . id) res

main = putStrLn $ formatTests testResults
