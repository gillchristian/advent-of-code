module Main where

import Data.List (sort, elemIndex)
import Data.List.Unique (count)

main :: IO ()
main = do
  s <- readFile "02.input.txt"
  let t = foldl search (0, 0) $ lines s
  print t
  print $ uncurry (*) t -- result

search :: (Integer, Integer) -> String -> (Integer, Integer)
search (c2, c3) line = (c2', c3')
  where tc = map snd . count $ line
        c2' = case elemIndex 2 tc of
                Just _ -> c2 + 1
                Nothing -> c2
        c3' = case elemIndex 3 tc of
                Just _ -> c3 + 1
                Nothing -> c3
