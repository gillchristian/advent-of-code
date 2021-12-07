module Day07 where

import Data.List (nub, sort)
import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = fmap read . splitOn ","

diff :: Num a => a -> a -> a
diff a b = abs $ a - b

sumToN :: Int -> Int
sumToN n = floor (fromIntegral (n * (n + 1)) / 2)

solve :: (Int -> Int -> Int) -> [Int] -> Int
solve f ns = 
  head 
    $ sort 
    $ fmap sum 
    $ fmap (\x -> fmap (f x) ns) 
    $ [(minimum ns)..(maximum ns)]

day07 :: IO ()
day07 = do
  ns <- parse <$> readFile "input/day07.txt"
  print $ solve diff ns
  print $ solve (\a b -> sumToN $ diff a b) ns
