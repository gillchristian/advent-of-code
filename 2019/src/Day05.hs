module Day05 where

import Paths_aoc2019 (getDataFileName)

-- Do the IO \o/

getInput :: IO String
getInput = readFile =<< getDataFileName "inputs/day-05.txt"

day05 :: IO ()
day05 = do
  _ <- getInput
  error "Day 05 not solved yet"
