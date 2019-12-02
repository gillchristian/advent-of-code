module Day01 where

import Paths_aoc2019 (getDataFileName)

type Mass = Int

type Fuel = Int

-- INPUT

parse :: String -> [Int]
parse = fmap read . lines

--- Calculations

fuelForMass :: Mass -> Fuel
fuelForMass mass
  | fuel > 0 = fuel
  | otherwise = 0
  where
    fuel = (mass `div` 3) - 2

fuelForMassFull :: Mass -> Fuel
fuelForMassFull = go 0
  where
    go :: Fuel -> Mass -> Fuel
    go totalFuel mass
      | mass <= 0 = totalFuel
      | otherwise = go (totalFuel + fuelForMass mass) (fuelForMass mass)

-- SOLUTIONS

part1 :: String -> Mass
part1 = sum . fmap fuelForMass . parse

part2 :: String -> Mass
part2 = sum . fmap fuelForMassFull . parse

-- do the IO \o/

day01 :: IO ()
day01 = do
  -- TODO: data-files
  input <- getDataFileName "inputs/day-01.txt" >>= readFile
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
