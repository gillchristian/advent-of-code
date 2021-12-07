module Day07 where

import Data.List (nub, sort)
import Data.List.Split (splitOn)

median :: (Ord a, Fractional a) => [a] -> a
median = fromSorted . sort
  where
  fromSorted :: Fractional a => [a] -> a
  fromSorted [] = error "Empty list"
  fromSorted [a] = a
  fromSorted [a,b] = ((a + b) / 2)
  fromSorted (a:xs) = fromSorted (init xs)

parse :: String -> [Int]
parse = fmap read . splitOn ","

diff :: Num a => a -> a -> a
diff a b = abs $ a - b

sumToN :: Int -> Int
sumToN n = (n * (n + 1)) `quot` 2

part1 :: [Int] -> Int
part1 ns = sum $ fmap (diff m) ns
  where
  m = floor $ median $ fmap fromIntegral ns

part2 :: [Int] -> Int
part2 ns = 
  head 
    $ sort 
    $ fmap sum 
      -- Naive solution, should memoize the diffs
      -- of each number to calculate only once
    $ fmap (\x -> fmap (sumToN . diff x) ns) 
    $ [(minimum ns)..(maximum ns)]

day07 :: IO ()
day07 = do
  ns <- parse <$> readFile "input/day07.txt"
  print $ part1 ns
  print $ part2 ns
