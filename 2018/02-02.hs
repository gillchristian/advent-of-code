module Main where

import Data.List (find)
import Data.Maybe
import Data.List.Utils (replace)

main :: IO ()
main = do
  s <- readFile "02-01.input.txt"
  let ls = lines s
  let (a, b) = case foldl (search ls) Nothing ls of
                Just r -> r
                Nothing -> ("Not", "found")
  putStrLn $ removeDiffByPos a b

search :: [String] -> Maybe (String, String) -> String -> Maybe (String, String)
search _ (Just r) _ = Just r
search ls Nothing s = case find (diffByOne s) ls of
                        Just r -> Just (s, r)
                        Nothing -> Nothing

diffByOne :: String -> String -> Bool
diffByOne a b = (==1) $ sum $ map tplDiff $ zip a b

removeDiffByPos :: String -> String -> String
removeDiffByPos a b = fst $ unzip $ filter tplEq $ zip a b

tplDiff :: (Eq a) => (a,a) -> Integer
tplDiff (a, b) 
  | a == b = 0
  | otherwise = 1

tplEq :: (Eq a) => (a,a) -> Bool
tplEq = uncurry (==)
