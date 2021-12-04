{-# LANGUAGE TupleSections #-}
module Day04 where

import Data.List.Split (splitOn)
import Data.List (transpose, find)
import Utils (zipMap, both, note)
import qualified System.Exit as Sys

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
countEpts = sum . map (sum . map unCell . filter isCell) . fst

calcSolution :: (Int, Board) -> Int
calcSolution (n, board) = n * countEpts board

findWinner :: [Int] -> [Board] -> Maybe (Int, Board)
findWinner [] _ = Nothing
findWinner _ [] = Nothing
findWinner (x:xs) boards =
  case find checkBoard next of
    Just win -> Just (x, win)
    Nothing -> findWinner xs next 
  where
  next = map (markNumber x) boards

findLastWinner :: [Int] -> [Board] -> Maybe (Int, Board)
findLastWinner [] boards = Nothing
findLastWinner _ [] = Nothing
findLastWinner (x:xs) boards =
  case zipMap checkBoard $ map (markNumber x) boards of
    [(board, True)] -> Just (x, board)
    res -> findLastWinner xs $ fst $ unzip $ filter (not . snd) res

part1 :: [Int] -> [Board] -> Maybe Int
part1 nums boards = calcSolution <$> findWinner nums boards

part2 :: [Int] -> [Board] -> Maybe Int
part2 nums boards = calcSolution <$> findLastWinner nums boards

day04 :: IO ()
day04 = do
  (nums, boards) <- parse <$> readFile "input/day04.txt"
  maybe (Sys.die "No solution") print $ part1 nums boards
  maybe (Sys.die "No solution") print $ part2 nums boards
