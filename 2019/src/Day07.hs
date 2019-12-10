module Day07 where

import Paths_aoc2019 (getDataFileName)

-- Do the IO \o/

getInput :: IO String
getInput = readFile =<< getDataFileName "inputs/day-07.txt"

day07 :: IO ()
day07 = do
  _ <- getInput
  error "Day 07 not solved yet"
