module Day03 where

import qualified System.Exit as Sys

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

parseDigit :: Char -> Word
parseDigit '1' = 1
parseDigit _ = 0

complementBinDigit :: Word -> Word
complementBinDigit 0 = 1
complementBinDigit _ = 0

complementBin :: [Word] -> [Word]
complementBin = map complementBinDigit

fromBinary :: [Word] -> Word
fromBinary = sum . zipWith (*) (map (2 ^) [0..]) . reverse

sumElems :: [[Word]] -> [Word]
sumElems = foldl (zipWith (+)) (repeat 0)

mostCommonDigits :: [[Word]] -> [Word]
mostCommonDigits bs = map mostCommon $ map fromIntegral $ sumElems bs
  where
  half = fromIntegral (length bs) / 2.0
  mostCommon n = if n >= half then 1 else 0

part1 :: [Word] -> (Word, Word)
part1 = (,) <$> fromBinary <*> (fromBinary . complementBin)

findRating :: Word -> [[Word]] -> Maybe [Word]
findRating criteria = go 0
  where
  go :: Int -> [[Word]] -> Maybe [Word]
  go _ [] = Nothing
  go _ [b] = Just b
  go i bs = go (i + 1) $ filter ((==) (mostCommon !! i) . (!! i)) bs
    where
    mostCommon = case criteria of
      0 -> complementBin $ mostCommonDigits bs
      1 -> mostCommonDigits bs
  
part2 :: [[Word]] -> Maybe (Word, Word)
part2 bs =
  fmap (both fromBinary) $ (,) <$> (findRating 1 bs) <*> (findRating 0 bs)

day03 :: IO ()
day03 = do
  bs <- fmap (fmap parseDigit) <$> lines <$> readFile "input/day03.txt"
  print $ uncurry (*) $ part1 $ mostCommonDigits bs
  maybe (Sys.die "No solution") (print . uncurry (*)) $ part2 bs
