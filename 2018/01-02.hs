module Main where

import Control.Monad.Loops
import Control.Monad.State.Lazy
import Data.Set (Set, empty, insert, member)

data Problem = Problem
  { input :: [String]
  , set :: Set Int
  , count :: Int
  }
  deriving Show

p :: String -> Problem 
p s = Problem 
        { input = cycle $ lines s
        , set = empty :: Set Int
        , count = 0
        }

next :: Problem -> (Maybe Int, Problem)
next Problem
  { input = input'
  , set = t
  , count = acc } = (res, ns)
    where x:xs = input'
          n = case x of
                ('+':num) -> (read num :: Int)
                ('-':num) -> -(read num :: Int)
                _ -> 0 :: Int
          c = acc + n
          (res, t') = case member c t of
                          True -> (Just c, t)
                          False -> (Nothing, insert c t)
          ns = Problem { input = xs
                       , set = t'
                       , count = c
                       }

search :: State Problem (Maybe Int)
search = do
  res <- state next
  case res of
    Just x -> return $ Just x
    Nothing -> search

main :: IO ()
main = do
  s <- readFile "01-01.input.txt"
  let res = evalState search $ p s
  print res
