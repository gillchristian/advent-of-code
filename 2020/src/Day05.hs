module Day05 where

import Data.List (sort)

toBinary :: Char -> Int
toBinary 'F' = 0
toBinary 'B' = 1
toBinary 'L' = 0
toBinary 'R' = 1

seatId :: [Int] -> Int
seatId [r1, r2, r3, r4, r5, r6, r7, c1, c2, c3] = r * 8 + c
  where
    r = r1 * 64 + r2 * 32 + r3 * 16 + r4 * 8 + r5 * 4 + r6 * 2 + r7
    c = c1 * 4 + c2 * 2 + c3

day05 :: IO ()
day05 = do
  input <- lines <$> readFile "input/day05.txt"
  let ids = sort $ fmap (seatId . fmap toBinary) input
  putStrLn $ "Part 01: " <> show (last ids) <> " is the highest ticket ID"
