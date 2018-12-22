import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- readFile "input.txt"
  -- Part One
  let counts = map getCounts $ lines input
  (putStrLn . show) $ (nCounts 2 counts) * (nCounts 3 counts)
  -- Part Two
  putStrLn $ tossDiff "" $ (findSimilar . lines) input

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

countDiff :: String -> String -> Integer -> Integer
countDiff (a:as) (b:bs) n = if a /= b
  then countDiff as bs (n + 1)
  else countDiff as bs n
countDiff _ _ n = n

findSimilarHelper :: String -> [String] -> Maybe [String]
findSimilarHelper a (b:ss) = if countDiff a b 0 == 1
  then Just [a, b]
  else findSimilarHelper a ss
findSimilarHelper a ss = Nothing

findSimilar :: [String] -> Maybe [String]
findSimilar (a:ss) = if similars == Nothing
  then findSimilar ss
  else similars
  where
    similars = findSimilarHelper a ss
findSimilar (ss) = Nothing

tossDiff :: String -> Maybe [String] -> String
tossDiff s (Just ((a:as):(b:bs):ss)) = if a == b
  then tossDiff (s ++ [a]) (Just [as, bs])
  else tossDiff s (Just [as, bs])
tossDiff s (Just (as:bs:ss)) = s
