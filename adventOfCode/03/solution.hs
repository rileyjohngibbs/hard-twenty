import Data.Char (isNumber)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let countedGrid = foldl (flip incrementGrid) (grid 1000 1000) (map lineToRect $ lines input)
  -- Part One
  (putStrLn . show) $ countTwoPlus countedGrid
  -- Part Two
  putStrLn $ (\(Rect i (Point x y) w h) -> i) $ head $ dropWhile (not . (flip noOverlap countedGrid)) $ map lineToRect $ lines input

data Point = Point Int Int deriving (Show)

data Rect = Rect String Point Int Int deriving (Show)

grid :: Int -> Int -> [[Int]]
grid w h = map (\y -> map (\x -> 0) [0..w]) [0..h]

-- String Example: "#13 @ 374,584: 10x10"

getId :: String -> (String, String)
getId s = (takeWhile ((/=) ' ') a, drop 2 b)
  where (a, b) = break ((==) '@') s

cornerDims :: String -> ([Char], [Char], [Char])
cornerDims s = (i, a, drop 2 b)
  where
    (i, shape) = getId s
    (a, b) = break ((==) ':') shape

lineToRect :: String -> Rect
lineToRect s = Rect i (Point
    (read x :: Int)
    (read $ drop 1 y :: Int)
  ) (read w :: Int) (read $ tail h :: Int)
  where
    (i, corner, dims) = cornerDims s
    (x, y) = break ((==) ',') corner
    (w, h) = break ((==) 'x') dims

incrementGrid :: Rect -> [[Int]] -> [[Int]]
incrementGrid (Rect i (Point x y) w h) grid =
  (take y grid)
  ++ (map (incrementRow (Rect i (Point x y) w h)) ((take h . drop y) grid))
  ++ (drop (y + h) grid)

incrementRow :: Rect -> [Int] -> [Int]
incrementRow (Rect i (Point x y) w h) ns =
  (take x ns)
  ++ (map ((+) 1) ((take w . drop x) ns))
  ++ (drop (x + w) ns)

countTwoPlus :: [[Int]] -> Int
countTwoPlus grid = foldl (\count row -> countTwoPlusRow row + count) 0 grid

countTwoPlusRow :: [Int] -> Int
countTwoPlusRow = foldl (\a b -> if b >= 2 then a + 1 else a) 0

noOverlap :: Rect -> [[Int]] -> Bool
noOverlap (Rect i (Point x y) w h) grid =
  all (\ns -> all ((==) 1) ns) $
      map
        (take w . drop x)
        ((take h . drop y) grid)
