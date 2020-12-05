module Day05 where

import Data.List (find, sort)
import Numeric

toBinary :: Char -> Int
toBinary c 
  | c == 'F' || c == 'L' = 0
  | c == 'B' || c == 'R' = 1

readSeatId :: String -> Int
readSeatId = fst . head . readInt 2 (const True) toBinary

findMissingSeat :: [Int] -> Maybe Int
findMissingSeat ids = fmap snd $ find (uncurry (/=)) $ zip ids [head ids ..]

day05 :: IO ()
day05 = do
  input <- lines <$> readFile "input/day05.txt"
  let ids = sort $ fmap readSeatId input
  putStrLn $ "Part 01: the highest is #" <> show (last ids)
  let (Just missing) = findMissingSeat ids
  putStrLn $ "Part 02: the missing is #" <> show missing
