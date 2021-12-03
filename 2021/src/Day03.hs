module Day03 where

import Data.Char (digitToInt)

flipBit :: Int -> Int
flipBit 0 = 1
flipBit _ = 0

fromBinary :: [Int] -> Int
fromBinary = sum . zipWith (*) (map (2 ^) [0..]) . reverse

sumElems :: [[Int]] -> [Int]
sumElems = foldl (zipWith (+)) (repeat 0)

mostCommon :: [[Int]] -> [Int]
mostCommon bs =
  map mostCommonBit $ map fromIntegral $ sumElems bs
  where
  half = fromIntegral (length bs) / 2.0
  mostCommonBit n = if n >= half then 1 else 0

part1 :: [[Int]] -> (Int, Int)
part1 bs = (,) <$> fromBinary <*> (fromBinary . map flipBit) $ mostCommon bs

findRating :: ([[Int]] -> [Int]) -> [[Int]] -> [Int]
findRating find = go 0
  where
  go :: Int -> [[Int]] -> [Int]
  go _ [b] = b
  go i bs = go (i + 1) $ filter ((== find bs !! i) . (!! i)) bs
  
part2 :: [[Int]] -> (Int, Int)
part2 bs =
  ( fromBinary $ findRating mostCommon bs
  , fromBinary $ findRating (map flipBit . mostCommon) bs
  )

day03 :: IO ()
day03 = do
  bs <- fmap (fmap digitToInt) <$> lines <$> readFile "input/day03.txt"
  print $ uncurry (*) $ part1 bs
  print $ uncurry (*) $ part2 bs
