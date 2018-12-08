module Day05 where

import           Data.Char
import           Data.Set  (fromList, toList)

main :: IO ()
main = do
  input <- readFile "input/05.txt"
  let clean = filter isLetter input
  print $ collapse clean
  print $ findBest clean

collapse :: String -> Int
collapse = go ""
  where
    go :: String -> String -> Int
    go [] (n:ns) = go [n] ns
    go (a:as) (b:bs)
      | a /= b && toLower a == toLower b = go as bs
      | otherwise = go (b : a : as) bs
    go collapsed [] = length collapsed

findBest :: String -> Int
findBest s =
  minimum .
  map (\c -> collapse . filter ((c /=) . toLower) $ s) . unique . map toLower $
  s

unique :: (Ord a) => [a] -> [a]
unique = toList . fromList
