module Day07 where

import           Data.List
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Graph as Graph
import           Data.Maybe
import           Debug.Trace
import           Text.Parsec

{-
Step C must be finished before step A can begin.
Step A must be finished before step B can begin.
Step C must be finished before step F can begin.
Step A must be finished before step D can begin.
Step F must be finished before step E can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.

Result: CABDFE
-}
type ID = Char
type Step = (ID, ID)

loadInput :: IO String
loadInput = readFile "input/07.txt"

-- parsing
step :: Parsec String () Step
step =
  (,) <$> (string "Step " *> letter <* string " must be finished before step ") <*>
  (letter <* string " can begin." <* char '\n')

parseInput :: String -> [Step]
parseInput input =
  case result of
    Right s -> s
    Left e  -> error $ show e
  where
    result = parse (many step) "Day 7 input" input

vertices :: [Step] -> [ID]
vertices = Set.toList . Set.fromList . tupleConcat

idKeyMap :: [Step] -> Map.Map Char Int
idKeyMap = Map.fromList . (\x -> zip x [0..]) . vertices

keyIdMap :: [Step] -> Map.Map Int Char
keyIdMap = Map.fromList . zip [0..] . vertices

graphEntry :: [Step] -> [(Char, Int, [Int])]
graphEntry steps = map f $ vertices steps 
  where 
    mp = idKeyMap steps
    f :: Char -> (Char, Int, [Int])
    f c = (c, k, ks)
      where look x = fromMaybe (error $ "Node '" ++ [x] ++ "' not in the list") $ Map.lookup x mp
            k = look c
            ks = map (look . snd) $ filter ((c==) . fst) steps

tupleConcat :: [(a, a)] -> [a]
tupleConcat xs = a ++ b
  where (a, b) = unzip xs

fstOf3 :: (a,b,c) -> a
fstOf3 (a,_,_) = a

traceThis :: (Show a) => String -> a -> a
traceThis prefix x = trace (prefix ++ " " ++ show x ++ "\n") x

-- solution

part1 :: [Step] -> (Graph.Graph, String)
part1 steps = (,) g $ map (fstOf3. lookup) . Graph.topSort $ g
  where (g,lookup,_) = Graph.graphFromEdges $ traceThis "graphEntry" $ graphEntry steps

main :: IO ()
main = do
  steps <- parseInput <$> loadInput
  let (g, sol) = part1 steps
  print g
  print $ "Actual:   " ++ sol
  print "Expected: CABDFE"
  print $ "Pass:     " ++ show (sol == "CABDFE")
{-
L ← Empty list that will contain the sorted elements
S ← Set of all nodes with no incoming edge
while S is non-empty do
    remove a node n from S
    add n to tail of L
    for each node m with an edge e from n to m do
        remove edge e from the graph
        if m has no other incoming edges then
            insert m into S
if graph has edges then
    return error   (graph has at least one cycle)
else
    return L   (a topologically sorted order)
-}
