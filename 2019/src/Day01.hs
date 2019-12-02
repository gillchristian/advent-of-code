module Day01 where

parse :: String -> [Int]
parse = fmap read . lines

formula :: Int -> Int
formula mass
  | fuel > 0 = fuel
  | otherwise = 0
  where
    fuel = (mass `div` 3) - 2

calculate :: Int -> Int
calculate mass = go mass 0
  where
    go :: Int -> Int -> Int
    go mass acc
      | mass <= 0 = acc
      | otherwise = go (formula mass) (acc + formula mass)

part1 :: String -> Int
part1 = sum . fmap formula . parse

part2 :: String -> Int
part2 = sum . fmap calculate . parse

day01 :: IO ()
day01 = do
  -- TODO: data-files
  input <- readFile "input/day-01"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
