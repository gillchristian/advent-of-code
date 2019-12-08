module Day03 where

import Data.Bifunctor (bimap)
import Data.List (find, sort)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
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
      { direction :: Direction,
        unsegment :: Int
      }
  deriving (Show)

type X = Int

type Y = Int

type Wire = [Segment]

type Wires = (Wire, Wire)

type Coord = (X, Y)

type Path = [Coord]

type BothPaths = (Path, Path)

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

followSegment :: Coord -> Path -> Segment -> (Coord, Path)
followSegment currentCoord wire (Segment _ 0) = (currentCoord, wire)
followSegment coord wire seg@(Segment dir n) =
  followSegment coord' (coord' : wire) $ Segment dir (dec n)
  where
    coord' = nextCoord coord seg

followWire :: Wire -> Path
followWire = go (0, 0) []
  where
    go :: Coord -> Path -> Wire -> Path
    go _ wire [] = reverse wire
    go currentCoord wire (nextSegment : rest) = go currentCoord' wire' rest
      where
        (currentCoord', wire') = followSegment currentCoord wire nextSegment

followWires :: Wires -> BothPaths
followWires = bimap followWire followWire

findIntersections :: BothPaths -> Set.Set Coord
findIntersections (w1, w2) =
  (Set.fromList $ w1) `Set.intersection` (Set.fromList $ w2)

-- UTILS

dec :: Num a => a -> a
dec = subtract 1

-- SOLUTIONS

part1 :: Set.Set Coord -> Int
part1 intersections = Set.elemAt 0 $ Set.map manhattanDistance intersections

part2 :: Set.Set Coord -> BothPaths -> Int
part2 intersections wires = head $ sort $ catMaybes $ map (mapper w2) w1
  where
    -- add the length & filter out the coordinates that arent intersections
    w1 = filter (\a -> Set.member (snd a) intersections) $ zip [1 ..] $ fst wires
    w2 = filter (\a -> Set.member (snd a) intersections) $ zip [1 ..] $ snd wires
    -- find the matching intersection
    finder :: [(Int, Coord)] -> (Int, Coord) -> Maybe (Int, Coord)
    finder w2' a = find (\b -> snd a == snd b) w2'
    -- get the distance of the intersection
    mapper :: [(Int, Coord)] -> (Int, Coord) -> Maybe Int
    mapper w2' a = (\b -> fst a + fst b) <$> finder w2' a

-- Do the IO \o/

getInput :: IO String
getInput = readFile =<< getDataFileName "inputs/day-03.txt"

day03 :: IO ()
day03 = do
  wires <- followWires <$> parseInput <$> getInput
  let intersections = findIntersections wires
  putStrLn $ "Part 01: " ++ (show $ part1 intersections)
  putStrLn $ "Part 02: " ++ (show $ part2 intersections wires)
