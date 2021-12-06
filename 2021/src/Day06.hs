module Day06 where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List.Split (splitOn)
import Data.Monoid (Sum(..))
import Utils (loopN)

parse :: String -> [Int]
parse = fmap read . splitOn ","

type State = HashMap Int (Sum Int)

age :: State -> State
age = HashMap.foldlWithKey insert HashMap.empty
  where
  insert state 0 v = HashMap.insertWith (<>) 6 v $ HashMap.insert 8 v state
  insert state k v = HashMap.insertWith (<>) (k - 1) v state

mkState :: [Int] -> State
mkState = foldl (\acc k -> HashMap.insertWith (<>) k (Sum 1) acc) HashMap.empty

solve :: Int -> [Int] -> Int
solve days = getSum . HashMap.foldl (<>) mempty . loopN days age . mkState

day06 :: IO ()
day06 = do
  fishes <- parse <$> readFile "input/day06.txt"
  print $ solve  80 fishes
  print $ solve 256 fishes
