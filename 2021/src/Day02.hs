module Day02 where

import Text.Read (readEither)
import qualified System.Exit as Sys

data Instruction
  = Up Int
  | Down Int
  | Fwd Int
  deriving (Show, Eq)

parse :: [String] -> Either String Instruction
parse ["up", n] = Up <$> readEither n
parse ["down", n] = Down <$> readEither n
parse ["forward", n] = Fwd <$> readEither n
parse ws = Left $ "Unexpected instruction: " <> unwords ws

move :: (Int, Int) -> Instruction -> (Int, Int)
move (x, y) (Up n) = (x - n, y)
move (x, y) (Down n) = (x + n, y)
move (x, y) (Fwd n) = (x, y + n)

aim :: (Int, Int, Int) -> Instruction -> (Int, Int, Int)
aim (x, y, aim) (Up n) = (x, y, aim - n)
aim (x, y, aim) (Down n) = (x, y, aim + n)
aim (x, y, aim) (Fwd n) = (x + n, y + aim * n, aim)

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
