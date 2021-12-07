module Day06 where

import Data.List.Split (splitOn)
import Utils (loopN)

parse :: String -> [Int]
parse = fmap read . splitOn ","

type State = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

age :: State -> State
age (a, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h + a, i, a)

sumState :: State -> Int
sumState (a, b, c, d, e, f, g, h, i) = a + b + c + d + e + f + g + h + i

mkState :: [Int] -> State
mkState input =
  ( length $ filter (== 0) input
  , length $ filter (== 1) input
  , length $ filter (== 2) input
  , length $ filter (== 3) input
  , length $ filter (== 4) input
  , length $ filter (== 5) input
  , length $ filter (== 6) input
  , length $ filter (== 7) input
  , length $ filter (== 8) input
  )

solve :: Int -> [Int] -> Int
solve days = sumState . loopN days age . mkState

day06 :: IO ()
day06 = do
  fishes <- parse <$> readFile "input/day06.txt"
  print $ solve  80 fishes
  print $ solve 256 fishes
