module Day03 where

import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Paths_aoc2019 (getDataFileName)

data Direction
  = U
  | D
  | R
  | L
  deriving (Show, Eq)

data Segment
  = Segment
      { dir :: Direction,
        unsegment :: Int
      }
  deriving (Show)

mapseg :: (Int -> Int) -> Segment -> Segment
mapseg f (Segment d n) = Segment d $ f n

instance Eq Segment where
  (==) a b = dir a == dir b

type X = Int

type Y = Int

type Wire = [Segment]

type Wires = (Wire, Wire)

type Coord = (X, Y)

manhattanDistance :: Coord -> Int
manhattanDistance (x, y) = abs x + abs y

-- PARSING

parseSegment :: String -> Segment
parseSegment ('U' : n) = Segment U $ read n
parseSegment ('D' : n) = Segment D $ read n
parseSegment ('R' : n) = Segment R $ read n
parseSegment ('L' : n) = Segment L $ read n
parseSegment s = error $ "invalid segment: '" ++ s ++ "'"

parseInput :: String -> Wires
parseInput input = (wire1, wire2)
  where
    (wire1 : wire2 : _) = fmap (fmap parseSegment . splitOn ",") $ lines input

-- Processing

followSegment :: Coord -> Set.Set Coord -> Segment -> (Coord, Set.Set Coord)
followSegment currentCoord wire (Segment _ (-1)) = (currentCoord, wire)
followSegment (x, y) wire seg =
  followSegment coord (Set.insert coord wire) $ mapseg dec seg
  where
    coord = case seg of
      Segment U _ -> (x, y + 1)
      Segment D _ -> (x, y -1)
      Segment R _ -> (x + 1, y)
      Segment L _ -> (x -1, y)

followWire :: Coord -> Set.Set Coord -> [Segment] -> Set.Set Coord
followWire _ wire [] = wire
followWire currentCoord wire (nextSegment : rest) =
  followWire currentCoord' wire' rest
  where
    (currentCoord', wire') =
      followSegment currentCoord wire $ mapseg dec nextSegment

-- UTILS

dec :: Num a => a -> a
dec = subtract 1

center :: Coord
center = (0, 0)

emptyWire :: Set.Set Coord
emptyWire = Set.singleton center

-- SOLUTIONS

part1 :: ([Segment], [Segment]) -> Int
part1 (w1, w2) =
  Set.elemAt 0
    $ Set.filter (/= 0)
    $ Set.map manhattanDistance
    $ (w1') `Set.intersection` w2'
  where
    w1' = followWire center emptyWire w1
    w2' = followWire center emptyWire w2

-- Do the IO \o/

day03 :: IO ()
day03 = do
  input <- parseInput <$> (readFile =<< getDataFileName "inputs/day-03.txt")
  putStrLn $ "Part 01: " ++ (show $ part1 input)
