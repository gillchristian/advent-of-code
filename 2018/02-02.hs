module Main where

import           Data.List  (find)
import           Data.Maybe

main :: IO ()
main = do
  s <- readFile "02.input.txt"
  let ls = lines s
  putStrLn $
    uncurry removeDiffByPos $
    fromMaybe ("Not", "found") $ foldl (search ls) Nothing ls

search :: [String] -> Maybe (String, String) -> String -> Maybe (String, String)
search _ (Just r) _ = Just r
search ls Nothing s =
  case find (diffByOne s) ls of
    Just r  -> Just (s, r)
    Nothing -> Nothing

diffByOne :: String -> String -> Bool
diffByOne a b = (== 1) $ sum $ zipWith diff a b

removeDiffByPos :: String -> String -> String
removeDiffByPos a b = map fst $ filter tplEq $ zip a b

diff :: (Eq a) => a -> a -> Integer
diff a b
  | a == b = 0
  | otherwise = 1

tplEq :: (Eq a) => (a, a) -> Bool
tplEq = uncurry (==)
