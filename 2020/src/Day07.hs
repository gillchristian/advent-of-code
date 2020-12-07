module Day07 where

import Control.Monad (void)
import Data.Function ((&))
import Data.Graph (Graph)
import qualified Data.Graph as Graph
import Data.Graph.DGraph (DGraph)
import qualified Data.Graph.DGraph as DGraph
import qualified Data.Graph.Types as DGraph
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser, parseFromFile)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

instance Functor ((,,) a b) where
  fmap f (a, b, c) = (a, b, f c)

allPaths :: Graph -> Graph.Vertex -> [Graph.Vertex]
allPaths graph vertex =
  graph
    & Graph.vertices
    & filter (/= vertex)
    & filter (flip (Graph.path graph) vertex)

color :: Parser String
color =
  (\a b -> a <> " " <> b)
    <$> P.many P.letter <* P.space
    <*> P.many P.letter

bagsOrBag = P.try (P.string "bags") <|> P.string "bag"

bagRule :: Parser (Int, String)
bagRule =
  (,)
    <$> (read <$> P.many P.digit) <* P.spaces
    <*> color <* P.spaces <* bagsOrBag

line :: Parser (String, [(Int, String)])
line = do
  nodeColor <- color
  void $ P.spaces *> P.string "bags contain" *> P.spaces
  containedBags <- P.try noOther <|> rules
  void $ P.char '.' <* P.spaces
  pure (nodeColor, containedBags)
  where
    rules = P.sepBy bagRule (P.char ',' <* P.spaces)
    noOther = [] <$ P.string "no other bags"

type WeightedEdge = (String, [(Int, String)])

type Edge = (String, [String])

unWeightEdge :: WeightedEdge -> Edge
unWeightEdge = (fmap . fmap) snd

allPathsToEdge :: [Edge] -> String -> Maybe Int
allPathsToEdge edges edge =
  length . fmap (fst3 . lookupNode) . allPaths graph <$> lookupVertext edge
  where
    (graph, lookupNode, lookupVertext) = Graph.graphFromEdges $ repeatFst <$> edges

repeatFst :: (a, b) -> (a, a, b)
repeatFst (a, b) = (a, a, b)

arcsFromWeightedEdges :: (String, [(Int, String)]) -> [DGraph.Arc String Int]
arcsFromWeightedEdges (from, tos) = fmap (uncurry (DGraph.Arc from) . swap) tos

countWeights :: DGraph String Int -> String -> Int
countWeights graph v = go [v]
  where
    go :: [String] -> Int
    go [] = 0
    go vs = total + go destVertices
      where
        vertices = DGraph.reachableAdjacentVertices' graph =<< vs
        total = sum $ DGraph.tripleAttribute <$> vertices
        destVertices = replicateVertices =<< vertices
        replicateVertices (_, destination, count) = replicate count destination

day07 :: IO ()
day07 = do
  (Right weightedEdges) <- parseFromFile (P.many line) "input/day07.txt"
  let edges = unWeightEdge <$> weightedEdges
      weightedGraph = DGraph.fromArcsList $ arcsFromWeightedEdges =<< weightedEdges
  putStrLn $ "Part 1: " <> show (fromMaybe 0 $ allPathsToEdge edges "shiny gold")
  putStrLn $ "Part 2: " <> show (countWeights weightedGraph "shiny gold")
