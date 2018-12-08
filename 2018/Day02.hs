module Day02 where

import           Data.List        (elemIndex, find, sort)
import           Data.List.Unique (count)
import           Data.Maybe

main :: IO ()
main = do
  s <- readFile "input/02.txt"
  let ls = lines s
  let t = foldl part1 (0, 0) ls
  print t
  print $ uncurry (*) t -- result
  putStrLn $
    uncurry removeDiffByPos $
    fromMaybe ("Not", "found") $ foldl (part2 ls) Nothing ls

part1 :: (Integer, Integer) -> String -> (Integer, Integer)
part1 (c2, c3) line = (c2', c3')
  where
    tc = map snd . count $ line
    c2' =
      case elemIndex 2 tc of
        Just _  -> c2 + 1
        Nothing -> c2
    c3' =
      case elemIndex 3 tc of
        Just _  -> c3 + 1
        Nothing -> c3

part2 :: [String] -> Maybe (String, String) -> String -> Maybe (String, String)
part2 _ (Just r) _ = Just r
part2 ls Nothing s =
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
