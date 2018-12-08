module Day08 where

import           Text.Parsec
import           Text.Parsec.Number

type Meta = [Int]

type Nodes = [Tree]

data Tree =
  Tree Nodes
       Meta
  deriving (Show)

loadInput :: IO String
loadInput = readFile "input/08.txt"

-- parsing
tree :: Parsec String () Tree
tree = do
  nodes <- int <* spaces
  meta <- int <* spaces
  Tree <$> count nodes tree <*> count meta (spaces *> int <* spaces)

parseInput :: String -> Tree
parseInput input =
  case parse tree "Day 8 input" input of
    Right ns -> ns
    Left e   -> error $ show e

-- solutions
sumMeta :: Tree -> Int
sumMeta (Tree [] meta)    = sum meta
sumMeta (Tree nodes meta) = sum meta + (sum . map sumMeta) nodes

countNodes :: Int -> Tree -> Int
countNodes total (Tree [] meta) = total + sum meta
countNodes total (Tree nodes meta) = total + foldl f 0 meta
  where
    ns = zip [0 ..] nodes
    f :: Int -> Int -> Int
    f acc i =
      case lookup (i - 1) ns of
        Just node -> countNodes acc node
        Nothing   -> acc

main :: IO ()
main = do
  t <- parseInput <$> loadInput
  print $ sumMeta t
  print $ countNodes 0 t
