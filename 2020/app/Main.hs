module Main where

import Control.Monad (mapM_)
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
  runDay =<< getLine

main :: IO ()
main = mapM_ runDay =<< Env.getArgs
