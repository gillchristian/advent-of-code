module Day08 where

import Paths_aoc2019 (getDataFileName)

-- Do the IO \o/

getInput :: IO String
getInput = readFile =<< getDataFileName "inputs/day-08.txt"

day08 :: IO ()
day08 = do
  _ <- getInput
  error "Day 08 not solved yet"
