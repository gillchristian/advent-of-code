module Main where

import Data.List (sort)
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
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
runDay x = do
  putStrLn $ "Day '" ++ x ++ "' not available yet ..."
  runDay =<< getLine

main :: IO ()
main = mapM_ runDay =<< sort <$> Env.getArgs
