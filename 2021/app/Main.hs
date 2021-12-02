module Main where

import Data.List (sort)
import Day01 (day01)
import Day02 (day02)
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
runDay x = do
  putStrLn $ "Day '" ++ x ++ "' not available yet ..."
  putStrLn "Please select another day"
  runDay =<< getLine

main :: IO ()
main = mapM_ runDay =<< sort <$> Env.getArgs
