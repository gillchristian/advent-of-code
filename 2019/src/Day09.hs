module Day09 where

import Paths_aoc2019 (getDataFileName)

-- Do the IO \o/

getInput :: IO String
getInput = readFile =<< getDataFileName "inputs/day-09.txt"

day09 :: IO ()
day09 = do
  _ <- getInput
  error "Day 09 not solved yet"
