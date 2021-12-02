module Day02 where

import Text.Read (readEither)
import qualified System.Exit as Sys

data Instruction
  = Vertical Int
  | Horizontal Int
  deriving (Show, Eq)

parse :: [String] -> Either String Instruction
parse ["up", n] = Vertical <$> negate <$> readEither n
parse ["down", n] = Vertical <$> readEither n
parse ["forward", n] = Horizontal <$> readEither n
parse ws = Left $ "Unexpected instruction: " <> unwords ws

move :: (Int, Int) -> Instruction -> (Int, Int)
move (x, y) (Vertical n) = (x + n, y)
move (x, y) (Horizontal n) = (x, y + n)

aim :: (Int, Int, Int) -> Instruction -> (Int, Int, Int)
aim (x, y, aim) (Vertical n) = (x, y, aim + n)
aim (x, y, aim) (Horizontal n) = (x + n, y + aim * n, aim)

fst2 :: (a, b, c) -> (a, b)
fst2 (a, b, _) = (a, b)

day02 :: IO ()
day02 = do
  result <- traverse (parse . words) <$> lines <$> readFile "input/day02.txt"
  case result of
    Right instructions -> do
      print $ uncurry (*) $ foldl move (0, 0) instructions
      print $ uncurry (*) $ fst2 $ foldl aim (0, 0, 0) instructions
    Left err -> Sys.die err
