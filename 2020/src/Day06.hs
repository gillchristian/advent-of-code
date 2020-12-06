{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

countUniqueChars :: [Text] -> Int
countUniqueChars =
  sum . fmap (length . nub . Text.unpack . Text.replace "\n" "")

countCommonChars :: [String] -> Int
countCommonChars lns = length $ filter id $ fmap allHaveChar ['a' .. 'z']
  where
    allHaveChar c = all (c `elem`) lns

countAllCommonChars :: [Text] -> Int
countAllCommonChars =
  sum . fmap (countCommonChars . fmap Text.unpack . Text.lines)

day06 :: IO ()
day06 = do
  input <- Text.splitOn "\n\n" <$> TextIO.readFile "input/day06.txt"
  putStrLn $ "Part 01: " <> show (countUniqueChars input)
  putStrLn $ "Part 02: " <> show (countAllCommonChars input)
