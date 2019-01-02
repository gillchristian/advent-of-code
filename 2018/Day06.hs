module Day06 where

-- import Data.Char
import           Data.Map.Strict (Map, fromList)

type ID = Int

data Cell
  = Empty
  | Pair ID
  | One ID
  | Many
  deriving (Show, Eq, Ord)

type Pair = (ID, ID)

type Grid = [Map ID Cell]

parse :: String -> [Pair]
parse = map (toPair . words . filterComma) . lines

filterComma :: String -> String
filterComma = filter (',' /=)

toPair :: [String] -> Pair
toPair [x, y] = (read x :: ID, read y :: ID)

-- distance from point (p1, p2) to point (q1, q2) is: |p1 - q1| + |p2 - q2|
(<->) :: Pair -> Pair -> Int
(p1, p2) <-> (q1, q2) = abs (p1 - q1) + abs (p2 - q2)

maxs :: [Pair] -> Pair
maxs ps = (x, y)
  where
    x = maximum . map fst $ ps
    y = maximum . map snd $ ps

grid :: Pair -> Grid
grid (x, y) = replicate x . fromList . map (flip (,) Empty) $ [0 .. y]

drawPairs :: [Pair] -> Grid -> Grid
drawPairs _ gd = gd

main :: IO ()
main = do
  input <- readFile "input/06.txt"
  print $ maxs . parse $ input
