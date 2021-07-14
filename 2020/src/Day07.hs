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

-- WIP ???
-- import Control.Applicative ((<|>))
-- import Data.Either (fromRight)
-- import Data.Graph.DGraph (DGraph, fromArcsList, inboundingArcs, isSink, isSource, outboundingArcs)
-- import Data.Graph.Types (Arc (..))
-- import Data.List (nub)
-- import Text.Parsec (char, digit, endBy, eof, letter, many1, optional, parse, sepBy, spaces, string, try)
-- import Text.Parsec.String (Parser)
-- import Text.Read (readMaybe)

-- type BagRule =
--   (String, [(String, Int)])

-- type BagGraph = DGraph String Int

-- target :: String
-- target = "shiny gold"

-- integer :: Parser Int
-- integer = do
--   digits <- many1 digit
--   case readMaybe digits of
--     Just n -> pure n
--     Nothing -> fail "expected int"

-- parseBagName :: Parser String
-- parseBagName = do
--   adj <- many1 letter <* spaces
--   color <- many1 letter <* spaces <* string "bag" <* optional (char 's')

--   pure $ adj <> " " <> color

-- destination :: Parser (String, Int)
-- destination = do
--   amount <- integer <* spaces
--   name <- parseBagName

--   pure (name, amount)

-- destinationList :: Parser [(String, Int)]
-- destinationList = sepBy destination (string ", ")

-- parseDestinations :: Parser [(String, Int)]
-- parseDestinations = try ([] <$ string "no other bags") <|> destinationList

-- parseBagRule :: Parser BagRule
-- parseBagRule = do
--   outer <- parseBagName <* spaces <* string "contain" <* spaces
--   destinations <- parseDestinations <* char '.'

--   pure (outer, destinations)

-- parseBagRules :: Parser [BagRule]
-- parseBagRules =
--   spaces *> endBy parseBagRule spaces <* eof

-- parseInput :: String -> [BagRule]
-- parseInput = fromRight [] . parse parseBagRules ""

-- ruleToArcs :: BagRule -> [Arc String Int]
-- ruleToArcs (source, destinations) = fmap (uncurry (Arc source)) destinations

-- buildGraph :: [BagRule] -> BagGraph
-- buildGraph = fromArcsList . foldMap ruleToArcs

-- indirectInbounds :: String -> BagGraph -> [String]
-- indirectInbounds name graph =
--   if isSource graph name
--     then []
--     else foldMap (\(Arc s _ _) -> s : indirectInbounds s graph) (inboundingArcs graph name)

-- outboundWeight :: String -> BagGraph -> Int
-- outboundWeight name graph =
--   if isSink graph name
--     then 1
--     else (+ 1) $ sum $ (\(Arc _ d n) -> n * outboundWeight d graph) <$> outboundingArcs graph name

-- part1 :: String -> Int
-- part1 = length . nub . indirectInbounds target . buildGraph . parseInput

-- part2 :: String -> Int
-- part2 = (-1 +) . outboundWeight target . buildGraph . parseInput

-- day07 :: IO ()
-- day07 = do
--   input <- readFile "input/day07.txt"

--   print $ part1 input
--   print $ part2 input
