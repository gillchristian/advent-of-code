{-# LANGUAGE ScopedTypeVariables #-}
module Day01 where

countIncreases :: [Int] -> Int
countIncreases xs = go 0 xs
  where
  go n (x:y:rest) = go (if y > x then n + 1 else n) (y:rest)
  go n [x] = n
  go n [] = n

countWindowIncreases :: [Int] -> Int
countWindowIncreases xs = go 0 xs
  where
  go n (a:b:c:d:rest) =
    let x = a + b + c
        y = b + c + d
     in go (if y > x then n + 1 else n) (b:c:d:rest)
  go n _ = n

day01 :: IO ()
day01 = do
  ls :: [Int] <- fmap read . lines <$> readFile "input/day01.txt"
  print $ countIncreases ls
  print $ countWindowIncreases ls
