module Day02 where

import Control.Monad (void)
import Text.Parsec as P
import Text.Parsec.Text (Parser, parseFromFile)

data Password
  = Password (Int, Int) Char String
  deriving (Eq, Show)

password :: Parser Password
password = do
  from <- read <$> P.many P.digit
  void $ P.char '-'
  to <- read <$> P.many P.digit
  char <- P.spaces *> P.letter
  void $ P.char ':' <* P.spaces
  pass <- P.many P.letter
  pure $ Password (from, to) char pass

solution1 :: [Password] -> Int
solution1 = length . filter inRange . map filterOtherChars
  where
    filterOtherChars (Password range c pass) = Password range c $ filter (== c) pass
    inRange (Password (from, to) _ pass) = length pass >= from && length pass <= to

solution2 :: [Password] -> Int
solution2 = length . filter policy
  where
    policy (Password (from, to) c pass) =
      (pass !! (from - 1) == c) /= (pass !! (to - 1) == c)

day02 :: IO ()
day02 = do
  (Right input) <- parseFromFile (P.endBy password P.spaces) "input/day02.txt"
  putStrLn $ ("Part 1: " <>) $ show $ solution1 input
  putStrLn $ ("Part 2: " <>) $ show $ solution2 input
