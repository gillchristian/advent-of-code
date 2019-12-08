module Main where

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import qualified System.Environment as Env

runDay :: [String] -> IO ()
runDay [] = putStrLn "Please select a day"
runDay ("01":_) = day01
runDay ("02":_) = day02
runDay ("03":_) = day03
runDay (x:_) = putStrLn $ "Day '" ++ x ++ "' not available yet ..."

main :: IO ()
main = runDay =<< Env.getArgs

