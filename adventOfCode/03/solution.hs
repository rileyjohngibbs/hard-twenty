import Data.Char (isNumber)

main :: IO ()
main = do
  input <- readFile "input.txt"
  -- Part One
  (putStrLn . show) $ countTwoPlus $ foldl (flip incrementGrid) (grid 1000 1000) (map lineToRect $ lines input)

data Point = Point Int Int deriving (Show)

data Rect = Rect Point Int Int deriving (Show)

grid :: Int -> Int -> [[Int]]
grid w h = map (\y -> map (\x -> 0) [0..w]) [0..h]

-- String Example: "#13 @ 374,584: 10x10"

dropId :: String -> String
dropId s = drop 2 b where (a, b) = break ((==) '@') s

cornerDims :: String -> ([Char], [Char])
cornerDims s = (a, drop 2 b)
  where (a, b) = break ((==) ':') (dropId s)

lineToRect :: String -> Rect
lineToRect s = Rect (Point (read x :: Int) (read $ drop 1 y :: Int)) (read w :: Int) (read $ tail h :: Int)
  where
    (corner, dims) = cornerDims s
    (x, y) = break ((==) ',') corner
    (w, h) = break ((==) 'x') dims

incrementRow :: Rect -> [Int] -> [Int]
incrementRow (Rect (Point x y) w h) ns = (take x ns) ++ (map ((+) 1) (take w (drop x ns))) ++ (drop (x + w) ns)

incrementGrid :: Rect -> [[Int]] -> [[Int]]
incrementGrid (Rect (Point x y) w h) grid = (take y grid) ++ (map (incrementRow (Rect (Point x y) w h)) (take h (drop y grid))) ++ (drop (y + h) grid)

countTwoPlus :: [[Int]] -> Int
countTwoPlus grid = foldl (\count row -> count + (foldl (\a b -> if b >= 2 then a + 1 else a) 0 row)) 0 grid
