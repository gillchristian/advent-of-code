module Main where

import Data.List (sort)
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
import Day09 (day09)
import qualified System.Environment as Env

runDay :: String -> IO ()
runDay "" = do
  putStrLn "Please select a day"
  runDay =<< getLine
runDay "01" = do
  putStrLn "Day 01"
  day01
runDay "02" = do
  putStrLn "Day 02"
  day02
runDay "03" = do
  putStrLn "Day 03"
  day03
runDay "04" = do
  putStrLn "Day 04"
  day04
runDay "05" = do
  putStrLn "Day 05"
  day05
runDay "06" = do
  putStrLn "Day 06"
  day06
runDay "07" = do
  putStrLn "Day 07"
  day07
runDay "08" = do
  putStrLn "Day 08"
  day08
runDay "09" = do
  putStrLn "Day 09"
  day09
runDay x = do
  putStrLn $ "Day '" ++ x ++ "' not available yet ..."
  putStrLn "Please select another day"
  runDay =<< getLine

main :: IO ()
main = mapM_ runDay =<< sort <$> Env.getArgs
