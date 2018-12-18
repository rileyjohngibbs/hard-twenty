import Data.Set (empty,insert,member,Set)
import System.Environment

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ show $ addLines $ lines input
  putStrLn $ show $ helperUpdateTotals empty 0 input

readLine :: String -> Integer
readLine s = if head s == '+'
  then read $ tail s :: Integer
  else read s :: Integer

addLines :: [String] -> Integer
addLines ss = sum $ map readLine $ ss

checkCache :: Ord a => a -> (Set a) -> (Either a (Set a))
checkCache x xs
  | member x xs = Left x
  | otherwise = Right (insert x xs)

updateTotals :: (Either Integer (Set Integer)) -> Integer -> [Integer] -> [Integer] -> Integer
updateTotals (Right totals) total (n:ns) orig = updateTotals (checkCache newTotal totals) newTotal ns orig
  where newTotal = total + n
updateTotals (Right totals) total ns orig = updateTotals (Right totals) total orig orig
updateTotals (Left dup) total ns orig = dup

helperUpdateTotals :: (Set Integer) -> Integer -> String -> Integer
helperUpdateTotals a b c = updateTotals (Right a) b d d
  where d = map readLine $ lines c
