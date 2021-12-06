{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Day05 where

import Control.Monad (join)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

spaces :: Parser ()
spaces = P.skipMany P.space

type Coord = (Int, Int)

data Line = Line Coord Coord deriving (Show, Eq)

parseCoord :: Parser Coord
parseCoord =
  (,) <$> P.decimal <* P.char ','
      <*> P.decimal

parseLine :: Parser Line
parseLine =
  Line <$> parseCoord <* spaces <* P.string "->" <* spaces
       <*> parseCoord

range :: (Ord a, Enum a, Num a) => a -> a -> [a]
range x y
  | x < y = [x..y]
  | otherwise = [x, x-1..y]

nonDiagonalCoords :: Line -> [Coord]
nonDiagonalCoords l@(Line (x1, y1) (x2, y2))
  | not (x1 == x2 || y1 == y2) = []
  | x1 == x2 = map (x1,) $ range y1 y2
  | y1 == y2 = map (,y1) $ range x1 x2

allCoords :: Line -> [Coord]
allCoords l@(Line (x1, y1) (x2, y2))
  | not (x1 == x2 || y1 == y2) = zip (range x1 x2) (range y1 y2)
  | x1 == x2 = map (x1,) $ range y1 y2
  | y1 == y2 = map (,y1) $ range x1 x2

type Grid = HashMap Coord Int

insertCoord :: Grid -> Coord -> Grid
insertCoord grid coord = HashMap.insertWith (+) coord 1 grid

solve :: (Line -> [Coord]) -> [Line] -> Int
solve coordsFromLine =
  length 
    . filter (>= 2)
    . HashMap.elems
    . foldl insertCoord HashMap.empty
    . (coordsFromLine =<<)

day05 :: IO ()
day05 = do
  lines <- P.parseOnly (parseLine `P.sepBy1` P.endOfLine) <$> BS.readFile "input/day05.txt"
  print $ solve nonDiagonalCoords <$> lines
  print $ solve allCoords <$> lines
