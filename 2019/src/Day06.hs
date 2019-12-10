module Day06 where

import Paths_aoc2019 (getDataFileName)

-- Do the IO \o/

getInput :: IO String
getInput = readFile =<< getDataFileName "inputs/day-06.txt"

day06 :: IO ()
day06 = do
  _ <- getInput
  error "Day 06 not solved yet"
