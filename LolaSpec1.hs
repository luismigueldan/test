module LolaSpec1
  ( readInput
  , readLine
  , formula
  )  where 

readInput :: [String] -> [[Int]]
readInput s =  map readLine s

readLine :: String -> [Int]
readLine l = map (read::String -> Int) (words l)


formula :: (Ord a) => [a] -> Bool
formula (a:b:c:xs) = a < b && b < c
formula _ = False


{- VERSION 0
readInput :: [String] -> [[Int]]
readInput s =  map readLine s

readLine :: String -> [Int]
readLine l = map (read::String -> Int) (words l)

echoLola :: [Int] -> String
echoLola e = "Input: " ++ (show e) ++ " => " ++ (show $ pseudoLOLA formula e) ++ "\n"

formula :: [Int] -> Bool
formula (a:b:c:xs) = a < b && b < c
formula _ = False
-}
