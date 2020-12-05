module Day01 where

day01 :: IO ()
day01 = do
  ls <- fmap read . lines <$> readFile "input/day01.txt"
  putStrLn $ ("Part 1 " <>) $ show $ head [y * x | x <- ls, y <- ls, y + x == 2020]
  putStrLn $ ("Part 2 " <>) $ show $ head [y * x * z | x <- ls, y <- ls, z <- ls, y + x + z == 2020]
