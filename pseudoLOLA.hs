import Control.Monad
import qualified Data.Char as C
import qualified LolaSpec1 as Spec

main = forever $ do
  interact f
  
f :: String -> String
f s =
  let stream = Spec.readInput $ lines s --reads and formats the input
  in show $ foldr (\x acc -> (fst acc || pseudoLOLA Spec.formula x , snd acc +1)) (False, 0) stream --produces output messsage applying the formula to the stream
  --this fold may be changed to behave like a trigger
  --foldr (\x acc -> echoLola x || acc) False stream --


--foldr (\x acc -> (fst acc ++ x, snd acc + 1) ) ("", 0 ) ["a","b","c"] works
  --will produce
--"Input: " ++ (show first) ++ " => " ++ (show $ pseudoLOLA formula first) ++
--"Input: " ++ (show second) ++ " => " ++ (show $ pseudoLOLA formula second)
--Input: [1,2,3] => Just True
--Input: [1,2,3] => Just False
echoLola :: (Ord a, Show a) => [a] -> String
echoLola e = "Input: " ++ (show e) ++ " => " ++ (show $ pseudoLOLA Spec.formula e) ++ "\n"

--applies formula to the stream, supposedly the stream is the value that the variables have at a given time t
pseudoLOLA :: ([a] -> b) -> [a] -> Maybe b
pseudoLOLA _ [] = Nothing
pseudoLOLA f xs = Just $ f xs

{- VERSION 1
f :: String -> String
f s =
  let stream = Spec.readInput $ lines s --reads and formats the input
  in foldr (\x acc -> echoLola x ++  acc) "" stream --produces output messsage applying the formula to the stream
  --this fold may be changed to behave like a trigger
  --foldr (\x acc -> echoLola x || acc) False stream --

  --will produce
--"Input: " ++ (show first) ++ " => " ++ (show $ pseudoLOLA formula first) ++
--"Input: " ++ (show second) ++ " => " ++ (show $ pseudoLOLA formula second)
--Input: [1,2,3] => Just True
--Input: [1,2,3] => Just False
echoLola :: (Ord a, Show a) => [a] -> String
echoLola e = "Input: " ++ (show e) ++ " => " ++ (show $ pseudoLOLA Spec.formula e) ++ "\n"

--applies formula to the stream, supposedly the stream is the value that the variables have at a given time t
pseudoLOLA :: ([a] -> b) -> [a] -> Maybe b
pseudoLOLA _ [] = Nothing
pseudoLOLA f xs = Just $ f xs

-}
{- VERSION 0
f :: String -> String
f s =
  let stream = Spec.readInput $ lines s
  in foldr (\x acc -> Spec.echoLola x ++ acc) "" stream

  --will produce
--"Input: " ++ (show first) ++ " => " ++ (show $ pseudoLOLA formula first) ++
--"Input: " ++ (show second) ++ " => " ++ (show $ pseudoLOLA formula second)

pseudoLOLA :: ([a] -> b) -> [a] -> Maybe b
pseudoLOLA _ [] = Nothing
pseudoLOLA f xs = Just $ f xs
-}

