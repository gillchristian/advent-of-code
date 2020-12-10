{-# LANGUAGE ScopedTypeVariables #-}

module Day09 where

findWeakness :: Int -> Int -> [Int] -> Maybe Int
findWeakness _ _ [] = Nothing
findWeakness preambleSize n xs = mbSolution
  where
    preamble = take preambleSize $ drop n xs
    a = xs !! (preambleSize + n)
    mbSolution =
      if null [x | x <- preamble, y <- preamble, x /= y && x + y == a]
        then Just a
        else findWeakness preambleSize (n + 1) xs

findSums :: [Int] -> Int -> Maybe (Int, Int)
findSums xs x = searchContiguous x acc 2 acc
  where
    acc = drop 1 $ scanl (+) 0 xs

searchContiguous :: Int -> [Int] -> Int -> [Int] -> Maybe (Int, Int)
searchContiguous target initial window ls
  | window > length initial = Nothing
  | null ls = searchContiguous target initial (window + 1) initial
  | otherwise =
    if sum (take window ls) == target
      then Just (0, 1)
      else searchContiguous target initial window $ drop 1 ls

day09 :: IO ()
day09 = do
  input <- fmap read . lines <$> readFile "input/day09.txt"
  let weakness = findWeakness 5 0 input
  putStrLn $ "Part 01: " <> show weakness
  putStrLn $ "Part 02: " <> show (findSums input =<< weakness)

-- initial 1 3 6 10 15
--
-- window  3
--
-- ls 1 3 6 10 15
--
-- target 19
