module Main where

import Control.Monad.Loops
import Control.Monad.State.Lazy

data Problem = Problem
  { input :: [String]
  , table :: [(Int, Int)]
  , count :: Int
  , index :: Int
  }
  deriving Show

p :: String -> Problem 
p s = Problem 
        { input = cycle $ lines s
        , table = []
        , count = 0
        , index = 0
        }

next :: Problem -> (Maybe Int, Problem)
next Problem
  { input = xs
  , table = t
  , count = acc
  , index = i } = (res, ns)
    where n = case xs !! i of
                ('+':num) -> (read num :: Int)
                ('-':num) -> -(read num :: Int)
                _ -> 0 :: Int
          c = acc + n
          (res, t') = case lookup c t of
                          Just _ -> (Just c, t)
                          Nothing -> (Nothing, (c, n):t)
          ns = Problem { input = xs
                       , table = t'
                       , count = c
                       , index = i + 1
                       }

search :: State Problem (Maybe Int)
search = do
  res <- state next
  -- return res
  case res of
    Just x -> return $ Just x
    Nothing -> search

main :: IO ()
main = do
  s <- readFile "01-01.input.txt"
  let res = evalState search $ p s
  print res
