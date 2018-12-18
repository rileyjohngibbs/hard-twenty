import Data.Set (empty,insert,member,Set)
import System.Environment

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ show $ addLines $ lines input
  putStrLn $ show $ helperg empty 0 input

readLine :: String -> Integer
readLine s = if head s == '+'
  then read $ tail s :: Integer
  else read s :: Integer

addLines :: [String] -> Integer
addLines ss = sum $ map readLine $ ss

addToFive :: Integer -> Integer -> [Integer] -> Integer
addToFive count total numbers = if total == 5
  then count
  else if length numbers > 0
    then addToFive (count + 1) (head numbers + total) (tail numbers)
    else -1

f :: Ord a => a -> (Set a) -> (Either a (Set a))
f x xs
  | member x xs = Left x
  | otherwise = Right (insert x xs)

g :: (Either Integer (Set Integer)) -> Integer -> [Integer] -> [Integer] -> Integer
g (Right ts) t (n:ns) orig = g (f (t + n) ts) (t + n) ns orig
g (Right ts) t ns orig = g (Right ts) t orig orig
g (Left dup) t ns orig = dup

helperg :: (Set Integer) -> Integer -> String -> Integer
helperg a b c = g (Right a) b d d
  where d = map readLine $ lines c
