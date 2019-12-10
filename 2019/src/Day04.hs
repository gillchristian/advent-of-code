module Day04 where

import Paths_aoc2019 (getDataFileName)

-- Do the IO \o/

getInput :: IO String
getInput = readFile =<< getDataFileName "inputs/day-04.txt"

day04 :: IO ()
day04 = do
  _ <- getInput
  error "Day 04 not solved yet"
