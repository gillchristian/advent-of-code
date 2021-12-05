{-# LANGUAGE TupleSections #-}
module Day04 where

import Control.Monad (join)
import Data.List.Split (splitOn)
import Data.List (transpose, find)
import qualified System.Exit as Sys
import Utils (zipMap, both, note)

data Cell
  = Ept {unCell :: Int}
  | Mrk {unCell :: Int}
  deriving (Show, Eq)

markCell :: Int -> Cell -> Cell
markCell i (Ept n)
  | i == n = Mrk n
  | otherwise = Ept n
markCell _ (Mrk n) = Mrk n

isMrk :: Cell -> Bool
isMrk (Mrk _) = True
isMrk _ = False

isCell :: Cell -> Bool
isCell = not . isMrk

type Line = [Cell]

type Row = Line
type Col = Line

type Board = ([Row], [Col])

parseBoard :: String -> Board
parseBoard input = (board, transpose board)
  where
  board = map (map Ept . map read . words) $ lines input

parse :: String -> ([Int], [Board])
parse input =
  ( map read $ splitOn "," nums
  , map parseBoard boards
  )
  where
  (nums:boards) = splitOn "\n\n" input

markNumber :: Int -> Board -> Board
markNumber n = both $ map $ map (markCell n)

checkLine :: Line -> Bool
checkLine = all isMrk

checkBoard :: Board -> Bool
checkBoard = uncurry (||) . both (any checkLine)

countEpts :: Board -> Int
countEpts = sum . map unCell . filter isCell . join . fst

calcSolution :: (Int, Board) -> Int
calcSolution (n, board) = n * countEpts board

type Strategy = [Int] -> [Board] -> Maybe (Int, Board)

findWinner :: Strategy
findWinner [] _ = Nothing
findWinner _ [] = Nothing
findWinner (x:xs) boards =
  case find checkBoard next of
    Just win -> Just (x, win)
    Nothing -> findWinner xs next 
  where
  next = map (markNumber x) boards

findLoser :: Strategy
findLoser [] boards = Nothing
findLoser _ [] = Nothing
findLoser (x:xs) boards =
  case zipMap checkBoard $ map (markNumber x) boards of
    [(board, True)] -> Just (x, board)
    next -> findLoser xs $ fst $ unzip $ filter (not . snd) next

solve :: [Int] -> [Board] -> Strategy -> Maybe Int
solve nums boards strat = calcSolution <$> strat nums boards

day04 :: IO ()
day04 = do
  (nums, boards) <- parse <$> readFile "input/day04.txt"
  maybe (Sys.die "No solution") print $ solve nums boards findWinner
  maybe (Sys.die "No solution") print $ solve nums boards findLoser
