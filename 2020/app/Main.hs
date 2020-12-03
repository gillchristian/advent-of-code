module Main where

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
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
runDay x = do
  putStrLn $ "Day '" ++ x ++ "' not available yet ..."
  runDay =<< getLine

main :: IO ()
main = mapM_ runDay =<< Env.getArgs
