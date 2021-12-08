module Day08 where

import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
import Data.Map ((!))
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Utils (toList4, unsafeFind, (<||>), (<&&>))
import qualified Utils.Parsing as P

type Segment = Char

segment :: Parser Segment
segment =
  P.char 'a' <|> P.char 'b' <|> P.char 'c' <|> P.char 'd' <|> P.char 'e' <|> P.char 'f' <|> P.char 'g'

type Digit = String

digit :: Parser Digit
digit = sort <$> P.many1 segment

type DigitOutput = (Digit, Digit, Digit, Digit)

digitOutput :: Parser DigitOutput
digitOutput =
  (,,,) <$> digit <* P.spaces
        <*> digit <* P.spaces
        <*> digit <* P.spaces
        <*> digit

type Line = ([Digit], DigitOutput)

line :: Parser Line
line =
  (,) <$> (digit `P.sepBy1` P.spaces) <* P.spaces <* P.char '|' <* P.spaces
      <*> digitOutput

parse :: BS.ByteString -> Either String [Line]
parse = P.parseOnly (line `P.sepBy1` P.endOfLine)

-- #1 -> 2 segments
-- #4 -> 4 segments
-- #7 -> 3 segments
-- #8 -> 7 segments
isEasyDigit :: Digit -> Bool
isEasyDigit = ((== 2) <||> (== 4) <||> (== 3) <||> (== 7)) . length

countEasyDigits :: DigitOutput -> Int
countEasyDigits = length . filter isEasyDigit . toList4

part1 :: [Line] -> Int
part1 = sum . fmap countEasyDigits . fmap snd

isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
isSubsetOf a b = all (`elem` b) a

deduceOutput :: Line -> Int
deduceOutput (signals, (a, b, c, d)) =
  read $ show =<< [m ! a, m ! b, m ! c, m ! d]
  where
  m = Map.fromList $ zip [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] [0..]

  overlaps1 = (n1 `isSubsetOf`)
  overlaps4 = (n4 `isSubsetOf`)
  overlaps6 = (`isSubsetOf` n6)
  overlaps7 = (n7 `isSubsetOf`)
  size5 = (== 5) . length
  size6 = (== 6) . length

  n1 = unsafeFind ((== 2) . length) signals
  n4 = unsafeFind ((== 4) . length) signals
  n7 = unsafeFind ((== 3) . length) signals
  n8 = unsafeFind ((== 7) . length) signals

  n0 = unsafeFind (size6 <&&> overlaps7 <&&> (not . overlaps4)) signals
  n6 = unsafeFind (size6 <&&> (not . overlaps4) <&&> (not . overlaps7) ) signals
  n9 = unsafeFind (size6 <&&> overlaps4 <&&> overlaps7) signals
  
  n2 = unsafeFind (size5 <&&> (not . overlaps1) <&&> (not . overlaps6)) signals
  n3 = unsafeFind (size5 <&&> overlaps7) signals
  n5 = unsafeFind (size5 <&&> overlaps6) signals

part2 :: [Line] -> Int
part2 = sum . fmap deduceOutput

day08 :: IO ()
day08 = do
  input <- parse <$> BS.readFile "input/day08.txt"
  print $ part1 <$> input
  print $ part2 <$> input
