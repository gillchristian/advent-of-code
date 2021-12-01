{-# LANGUAGE ScopedTypeVariables #-}
module Day01 where

sumWindow :: (Int, Int) -> [Int] -> Int
sumWindow (i, n) = sum . take n . drop i

countIncreases :: Int -> [Int] -> Int
countIncreases n xs = go xs 0
  where
  go xs c
    | length xs <= n = c
    | sumWindow (1, n) xs > sumWindow (0, n) xs = go (drop 1 xs) $ c + 1
    | otherwise = go (drop 1 xs) c

day01 :: IO ()
day01 = do
  ls :: [Int] <- fmap read . lines <$> readFile "input/day01.txt"
  print $ countIncreases 1 ls
  print $ countIncreases 3 ls

-- Solution by [and-pete](https://github.com/and-pete)
--
-- Compares just the non-overlapping elements
-- of the two sliding windows of Ints. e.g. in
-- the below sample
--   199  A      
--   200  A B    
--   208  A B
--   210    B 
-- ..the 200 and 208 are common to windows A + B
-- so you can just compare 210 to 199
solveApartN :: Int -> [Int] -> Int
solveApartN n xs =
    sum $ zipWith scoreIncrease xs (drop n xs)
  where
  scoreIncrease :: Int -> Int -> Int
  scoreIncrease old new
    | new > old = 1
    | otherwise = 0
