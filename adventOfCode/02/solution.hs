import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- readFile "input.txt"
  let counts = map getCounts $ lines input
  -- Part One
  (putStrLn . show) $ (nCounts 2 counts) * (nCounts 3 counts)

increment :: Map.Map Char Integer -> Char -> Map.Map Char Integer
increment m c = Map.alter maybeAdd c m

maybeAdd :: Maybe Integer -> Maybe Integer
maybeAdd (Just i) = Just (i + 1)
maybeAdd Nothing = Just 1

getCounts :: String -> Map.Map Char Integer
getCounts s = foldl increment Map.empty s

hasCount :: Integer -> Map.Map Char Integer -> Bool
hasCount n m = Map.foldl (\a b -> b == n || a) False m

countTrue :: [Bool] -> Integer
countTrue bs = foldl inc 0 bs
  where inc i b = if b then i + 1 else i

nCounts :: Integer -> [Map.Map Char Integer] -> Integer
nCounts n ms = countTrue $ map (hasCount n) ms
