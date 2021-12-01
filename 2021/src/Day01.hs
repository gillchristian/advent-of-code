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
