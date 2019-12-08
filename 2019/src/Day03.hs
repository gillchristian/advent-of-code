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
    rows = fmap (fmap parseSegment . splitOn ",") $ lines input
    wire1 = head rows
    wire2 = head $ tail rows

-- Processing

nextCoord :: Coord -> Segment -> Coord
nextCoord (x, y) (Segment U _) = (x, y + 1)
nextCoord (x, y) (Segment D _) = (x, y -1)
nextCoord (x, y) (Segment R _) = (x + 1, y)
nextCoord (x, y) (Segment L _) = (x -1, y)

followSegment :: Coord -> [Coord] -> Segment -> (Coord, [Coord])
followSegment currentCoord wire (Segment _ (-1)) = (currentCoord, wire)
followSegment coord wire seg = followSegment coord' (coord' : wire) seg'
  where
    coord' = nextCoord coord seg
    seg' = mapseg dec seg

followWire :: Coord -> [Coord] -> [Segment] -> [Coord]
followWire _ wire [] = wire
followWire currentCoord wire (nextSegment : rest) =
  followWire currentCoord' wire' rest
  where
    (currentCoord', wire') =
      followSegment currentCoord wire $ mapseg dec nextSegment

-- UTILS

dec :: Num a => a -> a
dec = subtract 1

-- SOLUTIONS

part1 :: ([Segment], [Segment]) -> Int
part1 segments =
  Set.elemAt 0
    $ Set.map manhattanDistance
    $ w1 `Set.intersection` w2
  where
    w1 = Set.fromList $ followWire (0, 0) [] (fst segments)
    w2 = Set.fromList $ followWire (0, 0) [] (snd segments)

-- Do the IO \o/

day03 :: IO ()
day03 = do
  input <- parseInput <$> (readFile =<< getDataFileName "inputs/day-03.txt")
  putStrLn $ "Part 01: " ++ (show $ part1 input)
