module Day02
    ( day2
    ) where

import Text.Parsec as P
import Text.Parsec.Text (parseFromFile, Parser)

data Password =
  Password (Int, Int) String 
  deriving (Eq, Show)

inputP :: Parser [Password]
inputP = undefined

day2 :: IO ()
day2 = do
   (Right input) <- parseFromFile inputP "input/day02.txt"
   print input
