module Main where

main :: IO ()
main = do
  s <- readFile "01-01.input.txt"
  print $ foldl f 0 $ lines s

f :: Integer -> String -> Integer
f acc ('+':n) = acc + (read n :: Integer)
f acc ('-':n) = acc - (read n :: Integer)
f acc _ = acc
